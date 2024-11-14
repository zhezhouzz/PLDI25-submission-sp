open Ast
open OcamlParser
open Parsetree
open Zdatatype
open To_id
open To_constant

let var_or_c_of_expr expr =
  match expr.pexp_desc with
  | Pexp_ident id -> RVar (longid_to_id id) #: None
  | _ -> RConst (To_constant.expr_to_constant expr)

let of_expr_aux label_of_expr expr =
  let parse_labels a =
    match a.pexp_desc with
    | Pexp_array es -> List.map label_of_expr es
    | _ -> _die_with [%here] "expected an array of lables "
  in
  let parse_ids a =
    match a.pexp_desc with
    | Pexp_array es -> List.map id_of_expr es
    | _ -> _die_with [%here] "expected an array of names "
  in
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_fun (_, _, arg, expr) ->
        let open To_notation in
        let q, qv = notation_of_expr arg in
        let body = aux expr in
        let qvty =
          match qv.ty with Nt.Ty_unknown -> _die [%here] | _ -> qv.ty
        in
        let qv =
          match q with
          | FA -> qv.x #: (RForall qvty)
          | EX -> qv.x #: (RExists qvty)
          | PI -> qv.x #: (RPi qvty)
        in
        RExpr (QFRegex { qv; body })
    | Pexp_apply (func, args) -> (
        match (id_of_expr_opt func, List.map snd args) with
        | Some "starA", [ e1 ] -> StarA (aux e1)
        | Some "not", [ e1 ] -> Extension (ComplementA (aux e1))
        | Some "mu", _ ->
            _failatwith [%here] "the recursive automata are disallowed"
        | Some "||", [ a; b ] -> LorA (aux a, aux b)
        | Some "-", [ a; b ] -> SyntaxSugar (SetMinusA (aux a, aux b))
        | Some "&&", [ a; b ] -> LandA (aux a, aux b)
        | Some "ctx", [ a; b ] ->
            let atoms = parse_labels a in
            Extension (Ctx { atoms; body = aux b })
        | Some "ctxOp", [ a; b ] ->
            let op_names = parse_ids a in
            SyntaxSugar (CtxOp { op_names; body = aux b })
        | Some "range", [ a ] ->
            let atoms = parse_labels a in
            MultiAtomic atoms
        | Some "repeat", [ a; b ] -> (
            let lit = To_lit.lit_of_expr a in
            let r = aux b in
            match lit with
            | AVar x -> RExpr (Repeat (x.x, r))
            | AC (I i) -> RepeatN (i, r)
            | _ ->
                _failatwith [%here]
                @@ spf "unknown repeat %s" (Pprintast.string_of_expression expr)
            )
        | _, args ->
            let args = List.map var_or_c_of_expr args in
            let func = aux func in
            let mk_apply func arg = RExpr (RApp { func; arg }) in
            List.fold_left mk_apply func args)
    | Pexp_let (_, [ v ], body) ->
        let lhs = (id_of_pattern v.pvb_pat) #: None in
        let rhs = var_or_c_of_expr v.pvb_expr in
        RExpr (RLet { lhs; rhs; body = aux body })
    | Pexp_sequence (a, b) -> SeqA (aux a, aux b)
    | Pexp_ident id -> (
        let id = To_id.longid_to_id id in
        match id with
        | "epsilonA" -> EpsilonA
        | "emptyA" -> EmptyA
        | "anyA" -> Extension AnyA
        | _ -> RExpr (RVar id #: None))
    | Pexp_constant _ | Pexp_array _ -> RExpr (RConst (expr_to_constant expr))
    | _ -> Atomic (label_of_expr expr)
  in
  aux expr

let of_expr label_of_expr expr =
  let rty = of_expr_aux label_of_expr expr in
  (* let rty = Syntax.RtyRaw.SRL.normalize_name rty in *)
  rty

let pprint_var_or_c = function
  | RConst c -> To_constant.layout_constant c
  | RVar x -> x.x
  | _ -> _die [%here]

let rec pprint_aux layout_ty layout_label = function
  | EmptyA -> ("∅", true)
  | EpsilonA -> ("ϵ", true)
  | Atomic se -> (layout_label se, true)
  | MultiAtomic atoms ->
      (spf "[%s]" (List.split_by " " layout_label atoms), true)
  | LorA (a1, a2) ->
      ( spf "%s%s%s"
          (p_pprint layout_ty layout_label a1)
          "∪"
          (p_pprint layout_ty layout_label a2),
        false )
  | LandA (a1, a2) ->
      ( spf "%s%s%s"
          (p_pprint layout_ty layout_label a1)
          "∩"
          (p_pprint layout_ty layout_label a2),
        false )
  | SeqA (a1, a2) ->
      ( spf "%s;%s"
          (p_pprint layout_ty layout_label a1)
          (p_pprint layout_ty layout_label a2),
        false )
  | StarA a -> (spf "%s*" (p_pprint layout_ty layout_label a), true)
  | DComplementA { atoms; body } ->
      ( spf "Ctx[%s]{%sᶜ}"
          (List.split_by " " layout_label atoms)
          (p_pprint layout_ty layout_label body),
        true )
  | RepeatN (x, r) ->
      (spf "repeat[%i]{%s}" x (p_pprint layout_ty layout_label r), true)
  | Extension r -> pprint_extension_aux layout_ty layout_label r
  | SyntaxSugar r -> pprint_sugar_aux layout_ty layout_label r
  | RExpr r -> pprint_expr_aux layout_ty layout_label r

and pprint_extension_aux layout_ty layout_label = function
  | ComplementA a -> (spf "%sᶜ" (p_pprint layout_ty layout_label a), true)
  | AnyA -> ("•", true)
  | Ctx { atoms; body } ->
      ( spf "Ctx[%s]{%s}"
          (List.split_by " " layout_label atoms)
          (p_pprint layout_ty layout_label body),
        true )

and pprint_sugar_aux layout_ty layout_label = function
  | CtxOp { op_names; body } ->
      ( spf "Ctx[%s]{%s}"
          (List.split_by " " (fun x -> x) op_names)
          (p_pprint layout_ty layout_label body),
        true )
  | SetMinusA (a1, a2) ->
      ( spf "%s\\%s"
          (p_pprint layout_ty layout_label a1)
          (p_pprint layout_ty layout_label a2),
        false )

and pprint_expr_aux layout_ty layout_label = function
  | RRegex r -> pprint_aux layout_ty layout_label r
  | RConst c -> (layout_constant c, true)
  | RVar x -> (x.x, true)
  | RApp { func; arg } ->
      ( spf "%s %s"
          (p_pprint layout_ty layout_label func)
          (p_pprint layout_ty layout_label (RExpr arg)),
        false )
  | RLet { lhs; rhs; body } ->
      ( spf "let %s = %s in %s" lhs.x
          (p_pprint layout_ty layout_label (RExpr rhs))
          (p_pprint layout_ty layout_label body),
        false )
  | Repeat (x, r) ->
      (spf "repeat[%s]{%s}" x (p_pprint layout_ty layout_label r), true)
  | QFRegex { qv; body } ->
      let res =
        match qv.ty with
        | RForall ty ->
            spf "∀(%s:%s).%s" qv.x (Nt.layout ty)
              (p_pprint layout_ty layout_label body)
        | RExists ty ->
            spf "∃(%s:%s).%s" qv.x (Nt.layout ty)
              (p_pprint layout_ty layout_label body)
        | RPi ty ->
            spf "Π(%s<:%s).%s" qv.x (Nt.layout ty)
              (p_pprint layout_ty layout_label body)
        | RForallC c ->
            spf "∀(%s ∈ %s).%s" qv.x (layout_constant c)
              (p_pprint layout_ty layout_label body)
        | RExistsC c ->
            spf "∃(%s ∈ %s).%s" qv.x (layout_constant c)
              (p_pprint layout_ty layout_label body)
      in
      (res, true)

and p_pprint layout_ty layout_label a =
  let str, is_p = pprint_aux layout_ty layout_label a in
  if is_p then str else spf "(%s)" str

and pprint layout_ty layout_label a = fst (pprint_aux layout_ty layout_label a)

let layout_regex = pprint

let layout_regex_expr layout_ty layout_label r =
  layout_regex layout_ty layout_label (RExpr r)

let layout = pprint

open Ast
open OcamlParser
open Sugar
open Zdatatype
open Parsetree
open To_id
open To_op
open To_constant
open Mutils
open Ast_helper

(* NOTE: drop type notation *)
let rec lit_to_expr expr =
  let aux expr =
    match expr with
    | AC c -> constant_to_expr c
    | AAppOp (op, args) -> mk_op_apply (op.x, List.map typed_lit_to_expr args)
    | ATu l -> Exp.tuple (List.map typed_lit_to_expr l)
    | AProj _ -> _die_with [%here] "unimp"
    | AVar x -> mkvar x.x
  in
  aux expr

and typed_lit_to_expr expr = lit_to_expr expr.x

let rec layout_lit_to_smtlib2 expr =
  let aux expr =
    match expr with
    | AC c -> To_constant.layout_constant c
    | AAppOp (op, args) ->
        let op = match op.x with "==" -> "=" | _ -> op.x in
        spf "(%s %s)" op (List.split_by " " layout_typed_lit_to_smtlib2 args)
    | ATu _ -> _die_with [%here] "unimp"
    | AProj _ -> _die_with [%here] "unimp"
    | AVar x -> x.x
  in
  aux expr

and layout_typed_lit_to_smtlib2 expr = layout_lit_to_smtlib2 expr.x

let layout_lit lit = Pprintast.string_of_expression @@ lit_to_expr lit
let layout = layout_lit
let layout_typed_lit lit = layout lit.x

open Nt

let constructor_const_opt c =
  match c with
  | "true" -> Some (B true)
  | "false" -> Some (B false)
  | "()" -> Some U
  | _ -> None

let rec lit_of_expr expr =
  match expr.pexp_desc with
  | Pexp_tuple es -> ATu (List.map typed_lit_of_expr es)
  | Pexp_constraint _ -> _die [%here]
  | Pexp_ident id -> AVar (longid_to_id id) #: Ty_unknown
  | Pexp_construct (c, args) -> (
      let args =
        match args with
        | None -> []
        | Some args -> (
            let args = typed_lit_of_expr args in
            match args.x with ATu es -> es | _ -> [ args ])
      in
      let op = longid_to_id c in
      match (constructor_const_opt op, args) with
      | Some c, _ -> AC c
      | _, _ -> AAppOp (op #: Ty_unknown, args))
  | Pexp_constant _ -> AC (expr_to_constant expr)
  | Pexp_let _ -> _die [%here]
  | Pexp_apply (func, args) ->
      let args = List.map (fun x -> typed_lit_of_expr @@ snd x) args in
      let func = typed_lit_of_expr func in
      let res =
        match func.x with
        | AVar f -> (
            match string_to_op_opt f.x with
            | Some _ -> AAppOp (f, args)
            | None -> _die [%here])
        | _ -> _die [%here]
      in
      res
  | Pexp_ifthenelse _ -> _die [%here]
  | Pexp_match _ -> _die [%here]
  | Pexp_fun _ -> _die [%here]
  | Pexp_sequence _ -> _die [%here]
  | _ ->
      raise
      @@ failwith
           (Sugar.spf "not imp client parsing:%s"
           @@ Pprintast.string_of_expression expr)

and typed_lit_of_expr expr =
  match expr.pexp_desc with
  | Pexp_constraint (expr, ty) -> (lit_of_expr expr) #: (Nt.core_type_to_t ty)
  | _ -> (lit_of_expr expr) #: Ty_unknown

let of_expr = lit_of_expr
let layout lit = Pprintast.string_of_expression @@ lit_to_expr lit

open Gen
open Fa

(* open Zutils *)
open CharAutomata

type fa = DFA of dfa | NFA of nfa | REGEX of (string * Str.regexp)

let string_of_charlist str = String.of_seq @@ List.to_seq str

let accept (fa : fa) (str : C.t list) =
  match fa with
  | DFA dfa -> dfa_accept dfa str
  | NFA nfa -> nfa_accept nfa str
  | REGEX (_, r) -> Str.string_match r (string_of_charlist str) 0

let layout (fa : fa) =
  match fa with
  | DFA dfa -> layout_dfa dfa
  | NFA nfa -> layout_nfa nfa
  | REGEX (str, _) -> str ^ "\n"

let test_regex_fa (times : int) (regex : raw_regex) =
  let fa = DFA (compile_raw_regex_to_dfa regex) in
  let strs =
    List.filter_map (fun n -> n)
    @@ QCheck.Gen.generate ~n:times (string_gen_from_regex regex)
  in
  let total = List.length strs in
  let () = Printf.printf "generated %i cases under %i times\n" total times in
  List.fold_left
    (fun res str ->
      res
      &&
      if not (accept fa str) then
        let () = Printf.printf "For regex:\n%s\n" (layout_raw_regex regex) in
        let () = Printf.printf "error: %s\n" (string_of_charlist str) in
        false
      else true)
    true strs

let test_fa_1 (f : bool -> bool -> bool) (times : int) (fa1 : fa) (fa2 : fa) =
  let strs = QCheck.Gen.generate ~n:times string_gen in
  List.fold_left
    (fun res str ->
      res
      &&
      let b1 = accept fa1 str in
      let b2 = accept fa2 str in
      if not (f b1 b2) then
        let () = Printf.printf "error: %s\n" (string_of_charlist str) in
        let () = Printf.printf "[f1 accept: %b]\n%s\n" b1 (layout fa1) in
        let () = Printf.printf "[f2 accept: %b]\n%s\n" b2 (layout fa2) in
        false
      else true)
    true strs

let test_fa_2 (f : bool -> bool -> bool -> bool) (times : int) (fa1 : fa)
    (fa2 : fa) (fa3 : fa) =
  let strs = QCheck.Gen.generate ~n:times string_gen in
  List.fold_left
    (fun res str ->
      res
      &&
      let b1 = accept fa1 str in
      let b2 = accept fa2 str in
      let b3 = accept fa3 str in
      if not (f b1 b2 b3) then
        let () = Printf.printf "error: %s\n" (string_of_charlist str) in
        let () = Printf.printf "fa1 accept: %b\n" b1 in
        let () = Printf.printf "fa2 accept: %b\n" b2 in
        let () = Printf.printf "fa3 accept: %b\n" b3 in
        false
      else true)
    true strs

let test_fa_equal = test_fa_1 ( == )
let test_fa_complement = test_fa_1 ( != )
let test_fa_intersection = test_fa_2 (fun a b c -> c == (a && b))
let test_fa_union = test_fa_2 (fun a b c -> c == (a || b))

let qc_test_compile_raw_regex_to_dfa_1 (num_regex : int) (times : int) =
  let regexs = QCheck.Gen.generate ~n:num_regex basic_raw_regex_gen in
  List.fold_left
    (fun res r ->
      res
      &&
      let () = Printf.printf "testing %s\n" (layout_raw_regex r) in
      if test_regex_fa times r then true
      else
        let () = Printf.printf "testing %s\n" (layout_raw_regex r) in
        false)
    true regexs

let qc_test_compile_raw_regex_to_dfa_2 (num_regex : int) (times : int) =
  let regexs = QCheck.Gen.generate ~n:num_regex basic_raw_regex_gen in
  List.fold_left
    (fun res r ->
      res
      &&
      let () = Printf.printf "testing %s\n" (layout_raw_regex r) in
      let fa = DFA (compile_raw_regex_to_dfa r) in
      let r' =
        REGEX (layout_raw_regex r, Str.regexp (raw_regex_to_str_regex r))
      in
      let b = test_fa_equal times r' fa in
      if not b then Printf.printf "testing %s\n" (layout_raw_regex r);
      b)
    true regexs

let qc_test_fa_equal_trans f (num_regex : int) (times : int) =
  let regexs = QCheck.Gen.generate ~n:num_regex basic_raw_regex_gen in
  List.fold_left
    (fun res r ->
      res
      &&
      let () = Printf.printf "testing %s\n" (layout_raw_regex r) in
      let dfa = compile_raw_regex_to_dfa r in
      let fa1 = DFA dfa in
      let fa2 = DFA (f dfa) in
      test_fa_equal times fa1 fa2)
    true regexs

let qc_test_fa_minimalize = qc_test_fa_equal_trans minimize
let qc_test_fa_normalize = qc_test_fa_equal_trans normalize_dfa
let qc_test_fa_complete = qc_test_fa_equal_trans (complete_dfa space)

let qc_test_fa_complement (num_regex : int) (times : int) =
  let regexs = QCheck.Gen.generate ~n:num_regex basic_raw_regex_gen in
  List.fold_left
    (fun res r ->
      res
      &&
      let () = Printf.printf "testing %s\n" (layout_raw_regex r) in
      let dfa = compile_raw_regex_to_dfa r in
      let fa1 = DFA dfa in
      let fa2 = DFA (complement_dfa space dfa) in
      test_fa_complement times fa1 fa2)
    true regexs

let qc_test_fa_intersection (num_regex : int) (times : int) =
  let regexs = QCheck.Gen.generate ~n:num_regex basic_raw_regex_gen in
  let rec aux = function
    | [] | [ _ ] -> true
    | r1 :: r2 :: rs ->
        let () =
          Printf.printf "testing %s and %s \n" (layout_raw_regex r1)
            (layout_raw_regex r2)
        in
        let dfa1 = compile_raw_regex_to_dfa r1 in
        let dfa2 = compile_raw_regex_to_dfa r2 in
        let dfa3 = intersect_dfa dfa1 dfa2 in
        if test_fa_intersection times (DFA dfa1) (DFA dfa2) (DFA dfa3) then
          aux rs
        else false
  in
  aux regexs

let qc_test_fa_union (num_regex : int) (times : int) =
  let regexs = QCheck.Gen.generate ~n:num_regex basic_raw_regex_gen in
  let rec aux = function
    | [] | [ _ ] -> true
    | r1 :: r2 :: rs ->
        let () =
          Printf.printf "testing %s and %s \n" (layout_raw_regex r1)
            (layout_raw_regex r2)
        in
        let dfa1 = compile_raw_regex_to_dfa r1 in
        let dfa2 = compile_raw_regex_to_dfa r2 in
        let dfa3 = union_dfa dfa1 dfa2 in
        if test_fa_union times (DFA dfa1) (DFA dfa2) (DFA dfa3) then aux rs
        else false
  in
  aux regexs

let qc_test_dfa_to_regex (num_regex : int) (times : int) =
  let regexs = QCheck.Gen.generate ~n:num_regex basic_raw_regex_gen in
  let rec loop rs =
    match rs with
    | [] -> true
    | r :: rs ->
        let dfa = compile_raw_regex_to_dfa r in
        let r' = dfa_to_reg (minimize dfa) in
        let dfa' = compile_raw_regex_to_dfa r' in
        let () = Printf.printf "r: %s\n" (layout_raw_regex r) in
        let () = Printf.printf "r': %s\n" (layout_raw_regex r') in
        if test_fa_equal times (DFA dfa) (DFA dfa') then loop rs else false
  in
  loop regexs

let check_emp (dfa : dfa) = StateSet.is_empty (minimize dfa).finals

let check_eq (dfa1 : dfa) (dfa2 : dfa) =
  let dfa1' = complement_dfa space dfa1 in
  let dfa2' = complement_dfa space dfa2 in
  check_emp (intersect_dfa dfa1 dfa2') && check_emp (intersect_dfa dfa1' dfa2)

let check_eq_raw_regex (r1 : raw_regex) (r2 : raw_regex) =
  check_eq (compile_raw_regex_to_dfa r1) (compile_raw_regex_to_dfa r2)

let qc_test_dfa_to_regex_2 (num_regex : int) =
  let regexs = QCheck.Gen.generate ~n:num_regex basic_raw_regex_gen in
  let rec loop rs =
    match rs with
    | [] -> true
    | r :: rs ->
        let dfa = compile_raw_regex_to_dfa r in
        let r' = dfa_to_reg (minimize dfa) in
        let dfa' = compile_raw_regex_to_dfa r' in
        let () = Printf.printf "r: %s\n" (layout_raw_regex r) in
        let () = Printf.printf "r': %s\n" (layout_raw_regex r') in
        if check_eq dfa dfa' then loop rs else false
  in
  loop regexs

let qc_test_dfa_to_unf_2 (num_regex : int) =
  let regexs = QCheck.Gen.generate ~n:num_regex basic_raw_regex_gen in
  let rec loop rs =
    match rs with
    | [] -> true
    | r :: rs ->
        let r' =
          union_normal_form_to_raw_regex
          @@ raw_regex_to_union_normal_form (fun c -> [ c ]) r
        in
        let () = Printf.printf "r: %s\n" (layout_raw_regex r) in
        let () = Printf.printf "r': %s\n" (layout_raw_regex r') in
        if check_eq_raw_regex r r' then loop rs else false
  in
  loop regexs

(* let%test _ = qc_test_dfa_to_regex 1000 100 *)
let%test _ = qc_test_dfa_to_unf_2 200
(* let%test _ = qc_test_dfa_to_regex_2 1000 *)
(* let%test _ = qc_test_compile_raw_regex_to_dfa_2 10 30 *)

(* let%test _ = *)
(*   let dfa = compile_raw_regex_to_dfa (Seq [Mul]) in *)

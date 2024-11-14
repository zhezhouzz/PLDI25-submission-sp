exception FailWithModel of string * Z3.Model.model

let _failwithmodel loc msg model =
  raise (FailWithModel (Printf.sprintf "[%s:%i] %s" loc msg, model))

let ctx =
  Z3.mk_context [ ("model", "true"); ("proof", "false"); ("timeout", "1999") ]

let _check axiom q =
  Check.(handle_check_res (fun () -> smt_neg_and_solve ctx axiom q))
(* let check_with_pre pres vc = _check pres vc *)

let check_implies_with_pre axiom a b = _check axiom (Implies (a, b))
let check = _check

let check_bool axiom vc =
  let runtime, res = Sugar.clock (fun () -> check axiom vc) in
  let () =
    Env.show_debug_stat @@ fun _ -> Printf.printf "check_bool: %f\n" runtime
  in
  res

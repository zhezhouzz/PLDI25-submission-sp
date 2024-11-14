open Sugar
open Prop
open Zdatatype

module LitSet = Set.Make (struct
  type t = Nt.nt lit

  let compare = compare_lit Nt.compare_nt
end)

module ConstSet = Set.Make (struct
  type t = constant

  let compare = compare_constant
end)

module TVSet = Set.Make (struct
  type t = (Nt.nt, string) typed

  let compare x y = String.compare x.x y.x
end)

module LitMap = Map.Make (struct
  type t = Nt.nt lit

  let compare = compare_lit Nt.compare_nt
end)

type desym_map = {
  global_lit2int : int LitMap.t;
  global_int2lit : Nt.nt lit IntMap.t;
  local_lit2int : int LitMap.t StrMap.t;
  local_int2lit : Nt.nt lit IntMap.t StrMap.t;
}

type desym_ctx = {
  global_vars : (Nt.nt, string) typed list;
  event_tyctx : (Nt.nt, string) typed list StrMap.t;
  global_ftab : Nt.nt lit list;
  local_ftab : Nt.nt lit list StrMap.t;
  desym_map : desym_map;
}

let print_desym_ctx { global_vars; global_ftab; local_ftab; _ } =
  let () =
    Pp.printf "@{<bold>global_vars:@} %s\n"
      (List.split_by_comma _get_x global_vars)
  in
  let () =
    Pp.printf "@{<bold>global_ftab:@} %s\n"
      (List.split_by_comma layout_lit global_ftab)
  in
  let () =
    StrMap.iter
      (fun op l ->
        Pp.printf "@{<bold>local[%s]_ftab:@} %s\n" op
          (List.split_by_comma layout_lit l))
      local_ftab
  in
  ()

module BlistSet = Set.Make (struct
  type t = bool list

  let compare = List.compare Bool.compare
end)

type fact = { global_fact : bool list; local_fact : BlistSet.t StrMap.t }

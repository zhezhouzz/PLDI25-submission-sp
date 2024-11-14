val ( == ) : 'a -> 'a -> bool
val ( > ) : int -> int -> bool
val ( - ) : int -> int -> int
val eInitAccount : < accountId : aid ; balance : int > [@@gen]

let eInitAccount ?l:(ac = (true : [%v: aid])) ?l:(ba = (v > 0 : [%v: int])) =
  ( starA (anyA - EInitAccount (accountId == ac)),
    EInitAccount (accountId == ac && balance == ba),
    [||] )

val eWithDrawReq : < rId : rid ; accountId : aid ; amount : int > [@@gen]

let eWithDrawReq ?l:(id = (true : [%v: rid])) ?l:(ac = (true : [%v: aid]))
    ?l:(am = (v > 0 : [%v: int])) =
  ( allA,
    EWithDrawReq (rId == id && accountId == ac && amount == am),
    [| EReadQuery (rId == id && accountId == ac && amount == am) |] )

val eWithDrawResp :
  < rId : rid ; accountId : aid ; balance : int ; status : bool >
[@@obs]

let eWithDrawResp ?l:(id = (true : [%v: rid])) ?l:(ac = (true : [%v: aid]))
    ?l:(ba = (true : [%v: int])) ?l:(st = (true : [%v: bool])) =
  ( allA,
    EWithDrawResp (rId == id && accountId == ac && balance == ba && status == st),
    [||] )

(** Events used to communicate between the bank server and the backend database *)

(* event: send update the database, i.e. the `balance` associated with the `accountId` *)
val eUpdateQuery : < accountId : aid ; balance : int > [@@obs]

let eUpdateQuery ?l:(ac = (true : [%v: aid])) ?l:(ba = (v > 0 : [%v: int])) =
  (allA, EUpdateQuery (accountId == ac && balance == ba), [||])

(* event: send a read request for the `accountId`. *)
val eReadQuery : < rId : rid ; amount : int ; accountId : aid > [@@obs]

let eReadQuery =
  [|
    (fun (ba : int) ?l:(id = (true : [%v: rid])) ?l:(am = (true : [%v: int]))
         ?l:(ac = (true : [%v: aid])) ->
      ( (allA;
         EInitAccount (accountId == ac && balance == ba);
         starA
           (anyA - EReadQuery (accountId == ac) - EInitAccount (accountId == ac))),
        EReadQuery (rId == id && amount == am && accountId == ac),
        [|
          EReadQueryResp
            (rId == id && amount == am && accountId == ac && balance == ba);
        |] ));
  |]

val eReadQueryResp :
  < rId : rid ; amount : int ; accountId : aid ; balance : int >
[@@obs]

let eReadQueryResp =
  [|
    (fun ?l:(id = (true : [%v: rid])) ?l:(am = (true : [%v: int]))
         ?l:(ac = (true : [%v: aid]))
         ?l:(ba = (v > 0 && not (v > am) : [%v: int])) ->
      ( allA,
        EReadQueryResp
          (rId == id && amount == am && accountId == ac && balance == ba),
        [| EWithDrawResp (rId == id && accountId == ac && not status) |] ));
  |]

let[@goal] bankWithdrawSuccess (ac : aid) =
  not
    (allA;
     EWithDrawResp (accountId == ac && not status);
     allA)

val ( == ) : 'a -> 'a -> bool
val ( > ) : int -> int -> bool

(** message between env and router *)

val eStartTxnReq : unit [@@gen]

let eStartTxnReq (id : tGid) =
  ( starA (anyA - EStartTxnRsp (gid == id)),
    EStartTxnReq true,
    [| EStartTxnRsp (gid == id) |] )

val eStartTxnRsp : < gid : tGid > [@@obsRecv]

let eStartTxnRsp ?l:(id = (true : [%v: tGid])) =
  (starA (anyA - EShardUpdateKeyReq (gid == id)), EStartTxnRsp (gid == id), [||])

val eReadReq : < gid : tGid ; key : tKey > [@@gen]

let eReadReq ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey])) =
  ( (starA (anyA - EStartTxnRsp (gid == id));
     EStartTxnRsp (gid == id);
     starA (anyA - EStartTxnRsp (gid == id))),
    EReadReq (gid == id && key == k),
    [| EShardReadKeyReq (gid == id && key == k) |] )

val eReadRsp :
  < gid : tGid
  ; key : tKey
  ; value : tVal
  ; status : (uNKNOWN * oK * aBORT[@tCmdStatus]) >
[@@obsRecv]

let eReadRsp ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey]))
    ?l:(va = (true : [%v: tVal]))
    ?l:(st = (true : [%v: (uNKNOWN * oK * aBORT[@tCmdStatus])])) =
  ( starA anyA,
    EReadRsp (gid == id && key == k && value == va && status == st),
    [||] )

val eUpdateReq : < gid : tGid ; key : tKey ; value : tVal > [@@gen]

let eUpdateReq ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey]))
    ?l:(va = (true : [%v: tVal])) =
  ( (starA (anyA - EStartTxnRsp (gid == id));
     EStartTxnRsp (gid == id);
     starA (anyA - EStartTxnRsp (gid == id))),
    EUpdateReq (gid == id && key == k && value == va),
    [| EShardUpdateKeyReq (gid == id && key == k && value == va) |] )

val eUpdateRsp :
  < gid : tGid
  ; key : tKey
  ; value : tVal
  ; status : (uNKNOWN * oK * aBORT[@tCmdStatus]) >
[@@obsRecv]

let eUpdateRsp ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey]))
    ?l:(va = (true : [%v: tVal]))
    ?l:(st = (true : [%v: (uNKNOWN * oK * aBORT[@tCmdStatus])])) =
  ( starA anyA,
    EUpdateRsp (gid == id && key == k && value == va && status == st),
    [||] )

val eCommitTxnReq : < gid : tGid > [@@gen]

let eCommitTxnReq ?l:(id = (true : [%v: tGid])) =
  ( (starA (anyA - EStartTxnRsp (gid == id));
     EStartTxnRsp (gid == id);
     starA (anyA - EStartTxnRsp (gid == id))),
    ECommitTxnReq (gid == id),
    [| EShardPrepareReq (gid == id) |] )

val eCommitTxnRsp :
  < gid : tGid
  ; txnstatus : (eRROR * aCTIVE * cOMMITTED * aBORTED[@tTxnStatus]) >
[@@obsRecv]

let eCommitTxnRsp ?l:(id = (true : [%v: tGid]))
    ?l:(txnst =
        (true : [%v: (eRROR * aCTIVE * cOMMITTED * aBORTED[@tTxnStatus])])) =
  (starA anyA, ECommitTxnRsp (gid == id && txnstatus == txnst), [||])

val eRollbackTxnReq : < gid : tGid > [@@gen]

let eRollbackTxnReq ?l:(id = (true : [%v: tGid])) =
  ( (starA (anyA - EStartTxnRsp (gid == id));
     EStartTxnRsp (gid == id);
     starA (anyA - EStartTxnRsp (gid == id))),
    ERollbackTxnReq (gid == id),
    [|
      EShardAbortTxn (gid == id);
      ECommitTxnRsp
        (gid == id
        && txnstatus
           == ("ABORT" : (eRROR * aCTIVE * cOMMITTED * aBORTED[@tTxnStatus])));
    |] )

(** message between router and shard *)

val eShardReadKeyReq : < gid : tGid ; key : tKey > [@@obs]

let eShardReadKeyReq =
  [|
    (fun (va : tVal) ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey])) ->
      ( (starA (anyA - EStartTxnRsp (gid == id));
         EStartTxnRsp (gid == id);
         starA (anyA - EShardAbortTxn (gid == id) - EStartTxnRsp (gid == id));
         EShardUpdateKeyReq (gid == id && key == k && value == va);
         starA (anyA - EShardAbortTxn (gid == id) - EStartTxnRsp (gid == id))),
        EShardReadKeyReq (gid == id && key == k),
        [|
          EShardReadKeyRsp
            (gid == id && key == k && value == va
            && status == ("OK" : (uNKNOWN * oK * aBORT[@tCmdStatus])));
        |] ));
  |]

val eShardReadKeyRsp :
  < gid : tGid
  ; key : tKey
  ; value : tVal
  ; status : (uNKNOWN * oK * aBORT[@tCmdStatus]) >
[@@obs]

let eShardReadKeyRsp ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey]))
    ?l:(va = (true : [%v: tVal]))
    ?l:(st = (true : [%v: (uNKNOWN * oK * aBORT[@tCmdStatus])])) =
  ( starA anyA,
    EShardReadKeyRsp (gid == id && key == k && value == va && status == st),
    [| EReadRsp (gid == id && key == k && value == va && status == st) |] )

val eShardUpdateKeyReq : < gid : tGid ; key : tKey ; value : tVal > [@@obs]

let eShardUpdateKeyReq =
  [|
    (fun ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey]))
         ?l:(va = (true : [%v: tVal])) ->
      ( (starA (anyA - EStartTxnRsp (gid == id));
         EStartTxnRsp (gid == id);
         starA (anyA - EShardAbortTxn (gid == id) - EStartTxnRsp (gid == id))),
        EShardUpdateKeyReq (gid == id && key == k && value == va),
        [|
          EShardUpdateKeyRsp
            (gid == id && key == k && value == va
            && status == ("OK" : (uNKNOWN * oK * aBORT[@tCmdStatus])));
        |] ));
  |]

val eShardUpdateKeyRsp :
  < gid : tGid
  ; key : tKey
  ; value : tVal
  ; status : (uNKNOWN * oK * aBORT[@tCmdStatus]) >
[@@obs]

let eShardUpdateKeyRsp ?l:(id = (true : [%v: tGid]))
    ?l:(k = (true : [%v: tKey])) ?l:(va = (true : [%v: tVal]))
    ?l:(st = (true : [%v: (uNKNOWN * oK * aBORT[@tCmdStatus])])) =
  ( starA anyA,
    EShardUpdateKeyRsp (gid == id && key == k && value == va && status == st),
    [| EUpdateRsp (gid == id && key == k && value == va && status == st) |] )

val eShardCommitTxn : < gid : tGid > [@@obs]

let eShardCommitTxn ?l:(id = (true : [%v: tGid])) =
  (starA anyA, EShardCommitTxn (gid == id), [||])

val eShardAbortTxn : < gid : tGid > [@@obs]

let eShardAbortTxn ?l:(id = (true : [%v: tGid])) =
  (starA anyA, EShardAbortTxn (gid == id), [||])

val eShardPrepareReq : < gid : tGid > [@@obs]

let eShardPrepareReq =
  [|
    (fun ?l:(id = (true : [%v: tGid])) ->
      ( (starA (anyA - EShardPrepareReq (gid == id) - EStartTxnRsp (gid == id));
         EStartTxnRsp (gid == id);
         starA
           (anyA
           - EShardAbortTxn (gid == id)
           - EShardPrepareReq (gid == id)
           - EStartTxnRsp (gid == id))),
        EShardPrepareReq (gid == id),
        [| EShardPrepareRsp (gid == id && bstatus) |] ));
    (fun ?l:(id = (true : [%v: tGid])) ->
      ( (starA (anyA - EShardPrepareReq (gid == id));
         EShardAbortTxn (gid == id);
         starA (anyA - EShardPrepareReq (gid == id))),
        EShardUpdateKeyReq (gid == id),
        [| EShardPrepareRsp (gid == id && not bstatus) |] ));
  |]

val eShardPrepareRsp : < gid : tGid ; bstatus : bool > [@@obs]

let eShardPrepareRsp =
  [|
    (fun ?l:(id = (true : [%v: tGid])) ?l:(bst = (not v : [%v: bool])) ->
      ( starA anyA,
        EShardPrepareRsp (gid == id && iff bstatus bst),
        [|
          ECommitTxnRsp
            (gid == id
            && txnstatus
               == ("ABORTED"
                    : (eRROR * aCTIVE * cOMMITTED * aBORTED[@tTxnStatus])));
          EShardAbortTxn (gid == id);
        |] ));
    (fun ?l:(id = (true : [%v: tGid])) ?l:(bst = (v : [%v: bool])) ->
      ( starA anyA,
        EShardPrepareRsp (gid == id && iff bstatus bst),
        [|
          ECommitTxnRsp
            (gid == id
            && txnstatus
               == ("COMMITTED"
                    : (eRROR * aCTIVE * cOMMITTED * aBORTED[@tTxnStatus])));
          EShardCommitTxn (gid == id);
        |] ));
  |]

let[@goal] readVisibity (id : tGid) (k : tKey) (v1 : tVal) (v2 : tVal) =
  not
    (starA anyA;
     EUpdateRsp
       (gid == id && key == k && value == v1
       && (not (value == v2))
       && status == ("OK" : (uNKNOWN * oK * aBORT[@tCmdStatus])));
     starA
       (anyA
       - EUpdateRsp
           (gid == id && key == k
           && status == ("OK" : (uNKNOWN * oK * aBORT[@tCmdStatus]))));
     EReadRsp
       (gid == id && key == k && value == v2
       && (not (value == v1))
       && status == ("OK" : (uNKNOWN * oK * aBORT[@tCmdStatus])));
     starA anyA)

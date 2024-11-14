val ( == ) : 'a -> 'a -> bool
val readReq : unit [@@gen]
val getReq : unit [@@obs]
val readRsp : < va : int > [@@obsRecv]
val writeReq : < va : int > [@@gen]
val putReq : < va : int > [@@obs]
val putRsp : < va : int ; stat : bool > [@@obs]
val writeRsp : < va : int ; stat : bool > [@@obsRecv]
val commit : unit [@@obs]
val abort : unit [@@obs]

let readReq = (allA, ReadReq true, [| GetReq true |])
let getReq = (starA (anyA - Commit true), GetReq true, [| ReadRsp (va == -1) |])
let readRsp ?l:(x = (true : [%v: int])) = (allA, ReadRsp (va == x), [||])

let writeReq ?l:(x = (true : [%v: int])) =
  (allA, WriteReq (va == x), [| PutReq (va == x) |])

let putReq ?l:(x = (true : [%v: int])) =
  (allA, PutReq (va == x), [| PutRsp (va == x) |])

let putRsp =
  [|
    (fun ?l:(x = (true : [%v: int])) ?l:(s = (v : [%v: bool])) ->
      ( allA,
        PutRsp (va == x && stat),
        [| WriteRsp (va == x && stat == s); Commit true |] ));
    (fun ?l:(x = (true : [%v: int])) ?l:(s = (not v : [%v: bool])) ->
      ( allA,
        PutRsp (va == x && stat == s),
        [| WriteRsp (va == x && not stat); Abort true |] ));
  |]

let commit = (allA, Commit true, [||])
let abort = (allA, Abort true, [||])

let writeRsp ?l:(x = (true : [%v: int])) ?l:(s = (v : [%v: bool])) =
  (allA, WriteRsp (va == x && stat == s), [||])

let[@goal] readAfterWrite (x : int) (y : int) =
  not
    (allA;
     WriteRsp (va == x && stat);
     starA (anyA - WriteRsp stat);
     ReadRsp (va == y && not (x == y));
     starA (anyA - ReadRsp true - WriteRsp true))

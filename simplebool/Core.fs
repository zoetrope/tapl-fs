[<AutoOpen>]
module Core

exception TypeError of string
exception NoRuleApplies

// シフト： 項 t の打ち切り値 c 以上の d 個のシフト
let ``↑`` d t =
  let rec walk c t =
    match t with
    | TmVar (x, n) -> if x >= c then TmVar (x + d, n + d) else TmVar (x, n + d)
    | TmAbs (x, ty, t1) -> TmAbs (x, ty, walk (c + 1) t1)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
    | (TmTrue as t) -> t
    | (TmFalse as t) -> t
    | TmIf (t1, t2, t3) -> TmIf (walk c t1, walk c t2, walk c t3)
  in walk 0 t

// 代入： 項 t における変数番号 j への項 s の代入 [j ↦ s] t
let ``↦`` j s t =
  let rec walk c t =
    match t with
    | TmVar (x, n) ->  if x = (j + c) then ``↑`` c s else TmVar (x, n)
    | TmAbs (x, ty, t1) -> TmAbs (x, ty, walk (c + 1) t1)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
    | (TmTrue as t) -> t
    | (TmFalse as t) -> t
    | TmIf (t1, t2, t3) -> TmIf (walk c t1, walk c t2, walk c t3)
  in walk 0 t

// β簡約： 項 s を +1 シフト、項 s を 項 t に代入、全体を -1 シフト
let termSubstTop s t = ``↑`` (-1) (``↦`` 0 (``↑`` 1 s) t)

let rec getbinding ctx i =
  try let (_, bind) = List.nth ctx i in bind
  with
  | Failure _ -> raise(TypeError("Variable lookup failure: offset: %d, ctx size:"))// %d")) (msg i (List.length ctx))
  
let getTypeFromContext ctx i =
  match getbinding ctx i with
  | VarBind tyT -> tyT
  | _ -> raise(TypeError("getTypeFromContext: Wrong kind of binding for variable "))// + index2name fi ctx i))


let rec isval ctx t =
  match t with
  | TmTrue _ -> true
  | TmFalse _ -> true
  | TmAbs (_, _, _) -> true
  | _ -> false
  
  
let rec eval1 ctx t =
  match t with
  | TmApp ((TmAbs (x, tyT11, t12)), v2) when isval ctx v2 -> termSubstTop v2 t12
  | TmApp (v1, t2) when isval ctx v1 -> let t2' = eval1 ctx t2 in TmApp (v1, t2')
  | TmApp (t1, t2) -> let t1' = eval1 ctx t1 in TmApp (t1', t2)
  | TmIf ((TmTrue _), t2, t3) -> t2
  | TmIf ((TmFalse _), t2, t3) -> t3
  | TmIf (t1, t2, t3) -> let t1' = eval1 ctx t1 in TmIf (t1', t2, t3)
  | _ -> raise NoRuleApplies
  
let rec eval ctx t =
  try let t' = eval1 ctx t in eval ctx t' with | NoRuleApplies -> t
  
let rec typeof ctx t =
  match t with
  | TmVar (i, _) -> getTypeFromContext ctx i
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x (VarBind tyT1) in
      let tyT2 = typeof ctx' t2 in TyArr (tyT1, tyT2)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2
      in
        (match tyT1 with
         | TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11
             then tyT12
             else raise(TypeError("parameter type mismatch"))
         | _ -> raise(TypeError("arrow type expected")))
  | TmTrue -> TyBool
  | TmFalse -> TyBool
  | TmIf (t1, t2, t3) ->
      if (typeof ctx t1) = TyBool
      then
        (let tyT2 = typeof ctx t2
         in
           if tyT2 = (typeof ctx t3)
           then tyT2
           else raise(TypeError("arms of conditional have different types")))
      else raise(TypeError("guard of conditional not a boolean"))
  


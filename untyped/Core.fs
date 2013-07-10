[<AutoOpen>]
module Core

exception NoRuleApplies

// シフト： 項 t の打ち切り値 c 以上の d 個のシフト
let ``↑`` d t =
  let rec walk c t =
    match t with
    | TmVar (x, n) -> if x >= c then TmVar (x + d, n + d) else TmVar (x, n + d)
    | TmAbs (x, t1) -> TmAbs (x, walk (c + 1) t1)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
  in walk 0 t

// 代入： 項 t における変数番号 j への項 s の代入 [j → s] t
let ``→`` j s t =
  let rec walk c t =
    match t with
    | TmVar (x, n) ->  if x = (j + c) then ``↑`` c s else TmVar (x, n)
    | TmAbs (x, t1) -> TmAbs (x, walk (c + 1) t1)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
  in walk 0 t

// β簡約： 項 s を +1 シフト、項 s を 項 t に代入、全体を -1 シフト
let termSubstTop s t = ``↑`` (-1) (``→`` 0 (``↑`` 1 s) t)



let rec isval ctx t = 
    match t with 
    | TmAbs (_, _) -> true
    | _ -> false

// 1ステップ評価関数
let rec eval1 ctx t =
  match t with
  | TmApp ((TmAbs (x, t12)), v2) when isval ctx v2 -> termSubstTop v2 t12 // ラムダ抽象にラムダ抽象を適用する場合は、β簡約する
  | TmApp (v1, t2) when isval ctx v1 -> let t2' = eval1 ctx t2 in TmApp (v1, t2') // 左側がラムダ抽象なら、右側だけ評価する
  | TmApp (t1, t2) -> let t1' = eval1 ctx t1 in TmApp (t1', t2) // 左側を評価する
  | _ -> raise NoRuleApplies
  
// 多ステップ評価関数 (評価できなくなるまで1ステップ評価を繰り返す)
let rec eval ctx t =
  try let t' = eval1 ctx t 
      in eval ctx t' 
  with | NoRuleApplies -> t


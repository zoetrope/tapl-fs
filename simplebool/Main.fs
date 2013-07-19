module Main

open FParsec
open Microsoft.FSharp.Reflection

let rec isnamebound ctx x =
  match ctx with
  | [] -> false
  | (y, _) :: rest -> if y = x then true else isnamebound rest x
  
let rec pickfreshname ctx x =
  if isnamebound ctx x
  then pickfreshname ctx (x + "'")
  else (((x, NameBind) :: ctx), x)


let rec printty tyT =
  match tyT with | tyT -> printty_ArrowType tyT
and printty_ArrowType tyT =
  match tyT with
  | TyArr (tyT1, tyT2) -> printty_AType tyT1 + " " + "->" + " " + printty_ArrowType tyT2
  | tyT -> printty_AType tyT
and printty_AType tyT =
  match tyT with
  | TyBool -> "Bool"
  | tyT -> "(" + printty tyT + ")"
    
let rec printtm ctx t =
  match t with
  | TmAbs (x, tyT1, t2) ->
      let (ctx', x') = pickfreshname ctx x
      in "λ " + x' + ":" + printty tyT1 + "." + " " + printtm ctx' t2
  | TmIf (t1, t2, t3) ->
      "if " + printtm ctx t1 + " " + "then " + printtm ctx t2 + " " + "else " + printtm ctx t3
  | t -> printtm_AppTerm ctx t
and printtm_AppTerm ctx t =
  match t with
  | TmApp (t1, t2) ->
       (printtm_AppTerm ctx t1) + " " + (printtm_ATerm ctx t2)
  | t -> printtm_ATerm ctx t
and printtm_ATerm ctx t =
  match t with
  | TmVar (x, n) ->
      if (ctxlength ctx) = n
      then (index2name ctx x)
      else "[bad index]"
  | TmTrue _ -> "true"
  | TmFalse _ -> "false"
  | t -> "(" + (printtm ctx t) + ")"
  
  

let printEval trans1 trans2 context x =
    match x with
      | Success(result, _, _)   -> 
        printfn "%A" (result)
        printfn "↓"
        try
            printfn "%s : %s" (printtm context (trans1 context result)) (printty (trans2 context result))
        with
            | TypeError(msg) -> printfn "TypeError: %s" msg
        printfn "----------------------------"
      | Failure(errorMsg, _, _) -> 
        printfn "Failure: %s" errorMsg
        printfn "----------------------------"

let evalTerm str =
  let context : Context =  []
  printfn "%A" str
  printfn "↓"
  str |> runParserOnString pTerm context ""
      |> printEval eval typeof context

evalTerm "true"
evalTerm "true true false"
evalTerm "λ x:Bool.x "
evalTerm "(λ x:Bool->Bool. if x false then true else false)"
evalTerm "(λ x:Bool. if x then false else true)"
evalTerm "(λ x:Bool->Bool. if x false then true else false) (λ x:Bool. if x then false else true)"

System.Console.ReadLine() |> ignore



module Main

open FParsec

let rec isnamebound ctx x =
  match ctx with
  | [] -> false
  | (y, _) :: rest -> if y = x then true else isnamebound rest x
  
let rec pickfreshname ctx x =
  if isnamebound ctx x
  then pickfreshname ctx (x + "'")
  else (((x, NameBind) :: ctx), x)

let rec printtm ctx t = 
    match t with
      | TmAbs(x,t1) ->
        let (ctx',x') = pickfreshname ctx x in
        "(λ " + x' + ". " +  printtm ctx' t1 + ")"
      | TmApp(t1, t2) ->
        "("+ printtm ctx t1 + " "+ printtm ctx t2+ ")"
      | TmVar(x,n) ->
        if ctxlength ctx = n then
            index2name ctx x
        else
            "[bad index]"

let printEval trans context x =
    match x with
      | Success(result, _, _)   -> 
        printfn "%s" (printtm context result)
        printfn "↓%A" trans
        printfn "%s" (printtm context (trans context result))
        printfn "----------------------------"
      | Failure(errorMsg, _, _) -> 
        printfn "Failure: %s" errorMsg
        printfn "----------------------------"

let evalTerm str =
  let context : Context =  []
  printfn "%A" str
  printfn "↓(parse)"
  str |> runParserOnString pTerm context ""
      |> printEval eval context

evalTerm "(λx. x) (λx. x x)"
evalTerm "(λy. y) (λz. z)"
evalTerm "λx. (λy. x y) x"
evalTerm "(λx.λy. x y) (λy. y)"

System.Console.ReadLine() |> ignore
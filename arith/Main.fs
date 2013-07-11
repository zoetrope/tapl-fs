module Main

open FParsec

let printEval trans x =
    match x with
      | Success(result, _, _)   -> 
        printfn "%A" result
        printfn "↓"
        printfn "%A" (trans result)
        printfn "----------------------------"
      | Failure(errorMsg, _, _) -> 
        printfn "Failure: %s" errorMsg
        printfn "----------------------------"

let evalTerm str =
  printfn "%A" str
  printfn "↓"
  str |> run pTerm
      |> printEval eval

evalTerm "true"
evalTerm "false"
evalTerm "if false then true else false"

evalTerm "0"
evalTerm "succ (pred 0)"
evalTerm "iszero (pred (succ (succ 0)))"

evalTerm "5"

System.Console.ReadLine() |> ignore
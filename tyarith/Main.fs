module Main

open FParsec
open Microsoft.FSharp.Reflection

let printTerm trans x =
    match x with
      | Success(result, _, _)   -> 
        printfn "%A" result
        printfn "↓"
        try
            printfn "%A" (trans result)
        with
            | TypeError(msg) -> printfn "TypeError: %s" msg
        printfn "----------------------------"
      | Failure(errorMsg, _, _) -> 
        printfn "Failure: %s" errorMsg
        printfn "----------------------------"

let typeofTerm str =
  printfn "%A" str
  printfn "↓"
  str |> run pTerm
      |> printTerm typeof

(* well typed *)
typeofTerm "true"
typeofTerm "false"
typeofTerm "if false then true else false"

typeofTerm "0"
typeofTerm "succ (pred 0)"
typeofTerm "iszero (pred (succ (succ 0)))"

typeofTerm "5"

(* type error *)
typeofTerm "pred false"
typeofTerm "if 0 then true else false"

System.Console.ReadLine() |> ignore



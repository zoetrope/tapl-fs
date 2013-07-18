module Main

open FParsec
open Microsoft.FSharp.Reflection


let printEval x =
    match x with
      | Success(result, _, _)   -> 
        printfn "%A" (result)
        printfn "↓"
        printfn "%A" (result)
        printfn "----------------------------"
      | Failure(errorMsg, _, _) -> 
        printfn "Failure: %s" errorMsg
        printfn "----------------------------"

let evalTerm str =
  let context : Context =  []
  printfn "%A" str
  printfn "↓"
  str |> runParserOnString pTerm context ""
      |> printEval

//evalTerm "true"

//evalTerm "true true false"

//evalTerm "λ x:Bool.x "
evalTerm "(λ x:Bool->Bool. if x false then true else false)"
//evalTerm "(λ x:Bool. if x then false else true)"
//evalTerm "(λ x:Bool->Bool. if x false then true else false) (λ x:Bool. if x then false else true)"
System.Console.ReadLine() |> ignore



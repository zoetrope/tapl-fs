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
  str |> run pFields
      |> printEval (fun x -> x)

evalTerm "key = value"
evalTerm "value1 ,value2, value3"
evalTerm "key = value,key2=value2, value3"


System.Console.ReadLine() |> ignore
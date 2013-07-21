module Main

open FParsec

let printEval ctx trans x =
    match x with
      | Success(result, _, _)   -> 
        printfn "%A" result
        printfn "↓"
        printfn "%A" (trans result)
        printfn "%A" ctx
        printfn "----------------------------"
      | Failure(errorMsg, _, _) -> 
        printfn "Failure: %s" errorMsg
        printfn "----------------------------"
        
let evalTerm parser str =
  let context : Context =  []
  printfn "%A" str
  printfn "↓"
  str |> runParserOnString parser context ""
      |> printEval context (fun x -> x)


evalTerm pField "key = 'abc'"
evalTerm pFields "key = value"
evalTerm pFields "value1 ,value2, value3"
evalTerm pFields "key = value,key2=value2, value3"

evalTerm pCase "<phisical=x> ⇒ x"
evalTerm pCases "<phisical=x> ⇒ x | <virtual=y> ⇒ y
 | <virtual=z> ⇒ z"

evalTerm pTerm "{key1=1,key2=2,3}"


evalTerm pTerm "(true;true;false;true)"
System.Console.ReadLine() |> ignore
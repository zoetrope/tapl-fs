[<AutoOpen>]
module Parser

open System.IO
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

let pTrue = stringReturn "true" TmTrue
let pFalse = stringReturn "false" TmFalse

let rec pNumber = parse{
    let! num = pint32
    let rec f n =
            match n with
            | 0 -> TmZero
            | n -> TmSucc(f (n - 1))
    return f(num)}

let rec pTerm : Parser<term, unit> = pIf <|> pAppTerm

and pATerm = pParen <|> pTrue <|> pFalse <|> pNumber

and pSucc = parse {
    let! _ = pstring "succ"
    do! spaces
    let! t =  pATerm
    return TmSucc(t)}

and pPred = parse {
    let! _ = pstring "pred"
    do! spaces
    let! t =  pATerm
    return TmPred(t)}

and pIsZero = parse {
    let! _ = pstring "iszero"
    do! spaces
    let! t =  pATerm
    return TmIsZero(t)}

and pAppTerm = pATerm <|> pSucc <|> pPred <|> pIsZero

and pParen = parse{
    let! _ = pstring "("
    let! t = pTerm
    let! _ = pstring ")"
    return t}

and pIf = parse{
    let! _ = pstring "if" 
    do! spaces
    let! cond = pTerm
    do! spaces
    let! _ = pstring "then"
    do! spaces
    let! trueTerm = pTerm
    do! spaces
    let! _ = pstring "else"
    do! spaces
    let! falseTerm = pTerm
    return TmIf(cond, trueTerm, falseTerm)}
   
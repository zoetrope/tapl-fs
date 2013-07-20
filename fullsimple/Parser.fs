[<AutoOpen>]
module Parser

open System
open System.IO
open FParsec
open FParsec.Primitives
open FParsec.CharParsers


let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    (many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier") .>> spaces

(*
let pTrue = (stringReturn "true" TmTrue) .>> spaces
let pFalse = (stringReturn "false" TmFalse) .>> spaces

let pBoolType = stringReturn "Bool" TyBool
let pStringType = stringReturn "String" TyString
let pUnitType = stringReturn "Unit" TyString
let pFloatType = stringReturn "Float" TyString
let pNatType = stringReturn "Nat" TyString

let rec pType = pArrowType
and pAType = pParenType <|>
    identifier <|>
    pBoolType <|>
    "<" pFieldTypes ">" <|>
    pStringType <|>
    pUnitType <|>
    "{" pFieldTypes "}" <|>
    pFloatType <|>
    pNatType

and pArrow = chainr1 pAType (pstring "->" >>% fun x1 x2 -> TyArr(x1, x2))

and pArrowType = attempt(pArrow) <|> pAType

and pParenType = parse{ 
    let! _ = pstring "("
    let! t = pType
    let! _ = pstring ")"
    return t}
    
let rec pTerm = pAppTerm <|> pIf <|> pCaseOfCases <|> pLambda <|> pLet <|> pLetRec

and pCaseOfCases = "case" pTerm pCases

and pLet = parse{
    let! _ = pstring "let" .>> spaces1
    let! v = identifier
    let! _ = pstring "=" .>> spaces
    let! left = pTerm 
    let! _ = pstring "in" .>> spaces1
    let! right = pTerm
    return TmLet(v,left,right)}
    

and pLetRec = parse {
    let! _ = pstring "letrec" .>> spaces1
    let! v = identifier 
    let! _ = pstring ":" .>> spaces
    let! t = pType
    let! _ = pstring  "=" .>> spaces 
    let! t1 = pTerm 
    let! _ = pstring "in" .>> spaces1
    let! t2 = pTerm
    return TmLet(v,TmFix(TmAbs(v,t,t1)),t2)}

and pAppTerm = pPathTerm <|>
    pAppTerm pPathTerm <|> 
    pFix pPathTerm <|> 
    pTimesFloat pPathTerm pPathTerm <|> 
    pSucc pPathTerm <|> 
    pPred pPathTerm <|> 
    pIsZero pPathTerm

and pAscribeTerm = pATerm "as" pType <|> pATerm

and pPathTerm = pPathTerm "." identifier <|> pPathTerm "." pint32 <|> pAscribeTerm

and pTermSeq = pTerm <|> pTerm ";" pTermSeq

and pATerm = pParenTermSeq <|>
    "inert" "[" pType "]" <|>
    pTrue <|> pFalse <|>
    "<" identifier "=" pTerm ">" "as" pType <|>
    ideintifier <|>
    pStringv <|>
    "unit" <|>
    "{" pFields "}" <|>
    pfloat <|>
    pint32

and pLambda = parse {
    let! _ = pstring "λ"
    do! spaces
    let! name = identifier
    let! ctx = getUserState
    do! spaces
    let! _ = pstring ":"
    do! spaces
    let! typ = pType
    do! updateUserState(addname name)
    do! spaces
    let! _ = pstring "."
    do! spaces
    let! term = pTerm
    do! setUserState(ctx)
    return TmAbs(name, typ, term)}
    
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

and pParen = parse{ 
    let! _ = pstring "("
    let! t = pTerm
    let! _ = pstring ")"
    do! spaces
    return t}

and pCases = chainr1 pCase (pstring "|" >>% fun x1 x2 -> x1::x2)

and pCase = parse {
    let! _ = pstring "<"
    let! v1 = identifier 
    let! _ = pstring "="
    let! v2 = identifier
    let! _ = pstring  ">" 
    let! _ = pstring "==>"
    let! t1 = pAppTerm
    return (v1, (v2, t1))}


*)
let pTerm = identifier



let pKeyValueField = parse {
    let! f = identifier 
    let! _ = pstring "=" .>> spaces
    let! t = pTerm
    return (f, t)}

let pValueField = parse {
    let! t = pTerm
    return ("", t)}

let pField = attempt(pKeyValueField) <|> pValueField

let addIndex i t =
    match t with
    | ("", v) -> ((i+1).ToString(), v)
    | (k, v) -> (k, v)

let pFields : Parser<(string * string) list, unit>= parse{
    let! fields = sepBy pField (pstring "," .>> spaces)
    match fields with
    | [] -> return []
    | x -> return x |> List.mapi(fun i t -> addIndex i t)}
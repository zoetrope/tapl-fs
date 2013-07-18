[<AutoOpen>]
module Parser

open System
open System.IO
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

exception NoMatchError

type binding = | NameBind

let addbinding ctx x bind = (x, bind) :: ctx
  
let addname ctx x = addbinding x ctx NameBind

type Context = (string * binding) list

type Parser<'a> = Parser<'a, Context>

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

let ctxlength ctx = List.length ctx

let index2name ctx x =
  try let (xn, _) = List.nth ctx x in xn
  with
  | :? ArgumentException -> raise NoMatchError

// ただの関数ではなくパーサに変更。パース失敗時にfailを返したいから。
let rec name2index ctx x =
  match ctx with
  | [] -> fail "No match error"
  | (y, _) :: rest -> parse{ 
      if y = x then return 0
      else
        let! index = (name2index rest x)
        return   1 + index
    }
    
let pTrue = stringReturn "true" TmTrue
let pFalse = stringReturn "false" TmFalse
let pBool = stringReturn "Bool" TyBool

let pTmVar = 
    parse {
        let! name = identifier
        let! ctx = getUserState
        let! index = name2index ctx name 
        return TmVar(index, ctxlength ctx)
    }

let rec pType = pArrowType
and pAType = pParenType <|> pBool

and pArrow = chainl1 pAType (pstring "->" >>% fun x1 x2 -> TyArr(x1, x2))

and pArrowType = attempt(pArrow) <|> pAType

and pParenType = parse{ 
    let! _ = pstring "("
    let! t = pType
    let! _ = pstring ")"
    return t}


let rec pTerm = pAppTerm <|> pLambda <|> pIf

// TODO: if x false then みたいなのがパースできない。falseの後ろのスペースを適用のスペースとして処理されてしまう
and pAppTerm = chainl1 pATerm (pstring " " >>% fun e1 e2 -> TmApp(e1,e2))

and pATerm = pParen <|> attempt(pTmVar) <|> pTrue <|> pFalse

and pLambda = parse {
    let! _ = pstring "λ"
    do! spaces
    let! name = identifier
    let! ctx = getUserState
    do! updateUserState(addname name)
    do! spaces
    let! _ = pstring ":"
    do! spaces
    let! typ = pType
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
    return t}



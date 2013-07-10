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
  
let rec name2index ctx x =
  match ctx with
  | [] -> raise NoMatchError
  | (y, _) :: rest -> if y = x then 0 else 1 + (name2index rest x)

let pTmVar = parse {
    let! name = identifier
    let! ctx = getUserState
    return TmVar(name2index ctx name, ctxlength ctx)
}

let rec pTerm = pLambda <|> pAppTerm

and pAppTerm = attempt(pApp) <|> pATerm

and pATerm = pParen <|> pTmVar

(*
// 左再帰が無限ループに陥ってしまう
// http://d.hatena.ne.jp/kazu-yamamoto/20110127/1296098875
and pApp = parse {
    let! e1 = pAppTerm
    do! spaces1
    let! e2 = pATerm
    return TmApp(e1,e2)}
*)

// chainl1を使うと左再帰を処理できる
and pApp = chainl1 pATerm (spaces1 >>% fun e1 e2 -> TmApp(e1,e2))

and pLambda = parse {
    let! _ = pstring "λ"
    do! spaces
    let! name = identifier
    let! ctx = getUserState
    do! updateUserState(addname name) // pTermを処理する前に呼ぶ必要がある。
    do! spaces
    let! _ = pstring "."
    do! spaces
    let! t = pTerm
    do! setUserState(ctx) // pTermの解析が終わったらcontextを元に戻す
    return TmAbs(name, t)}

and pParen = parse{ 
    let! _ = pstring "("
    let! t = pTerm
    let! _ = pstring ")"
    return t}



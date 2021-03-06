﻿[<AutoOpen>]
module Parser

open System
open System.IO
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

exception NoMatchError

type Context = (string * binding) list

let addbinding ctx x bind = (x, bind) :: ctx
  
let addname x ctx = addbinding ctx x NameBind

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    (many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier") .>> spaces
    
let pTrue = (stringReturn "true" TmTrue) .>> spaces
let pFalse = (stringReturn "false" TmFalse) .>> spaces


let pUnit = (stringReturn "unit" TmUnit) .>> spaces

let ctxlength ctx = List.length ctx

let index2name ctx x =
  try let (xn, _) = List.nth ctx x in xn
  with
  | :? ArgumentException -> raise NoMatchError

let rec name2index ctx x =
  match ctx with
  | [] -> fail "No match error"
  | (y, _) :: rest -> parse{ 
      if y = x then return 0
      else
        let! index = (name2index rest x)
        return   1 + index
    }
    
let addIndex i t =
    match t with
    | ("", v) -> ((i+1).ToString(), v)
    | (k, v) -> (k, v)





let pTmVar = parse {
    let! name = identifier .>> spaces
    let! ctx = getUserState
    let! index = name2index ctx name 
    return TmVar(index, ctxlength ctx)}

let pStringV = parse {
    let! str = between (pstring "'") (pstring "'") (manyChars (noneOf "'"))
    return TmString(str)}

let pFloatV : Parser<term,Context> = parse {
    let! f = pfloat
    return TmFloat(f)}

let pIntV : Parser<term,Context> = parse{
    let! num = pint32
    let rec f n =
            match n with
            | 0 -> TmZero
            | n -> TmSucc(f (n - 1))
    return f(num)}
    
    
let pBoolType = stringReturn "Bool" TyBool
let pStringType = stringReturn "String" TyString
let pUnitType = stringReturn "Unit" TyString
let pFloatType = stringReturn "Float" TyString
let pNatType = stringReturn "Nat" TyString


let rec isnamebound ctx x =
  match ctx with
  | [] -> false
  | (y, _) :: rest -> if y = x then true else isnamebound rest x
  

let rec pType = pArrowType

and pAType =
    pParenType <|>
    pVarType <|>
    pBoolType <|>
    pVariantType <|>
    pStringType <|>
    pUnitType <|>
    pRecordType <|>
    pFloatType <|>
    pNatType

and pVariantType = parse {
    let! types = between (pstring "<") (pstring ">") pFieldTypes
    return TyVariant(types)}

and pRecordType = parse {
    let! types = between (pstring "{") (pstring "}") pFieldTypes
    return TyRecord(types)}

and pVarType = parse {
    let! name = identifier
    let! ctx = getUserState
    if isnamebound ctx name then
        let! index = name2index ctx name
        return TyVar(index, ctxlength ctx)
    else
        return TyId(name)}

and pArrow = chainr1 pAType (pstring "->" >>% fun x1 x2 -> TyArr(x1, x2))

and pArrowType = attempt(pArrow) <|> pAType

and pParenType = parse{ 
    let! _ = pstring "("
    let! t = pType
    let! _ = pstring ")"
    return t}
    
and pKeyValueFieldType = parse {
    let! f = identifier 
    let! _ = pstring ":" .>> spaces
    let! t = pType
    return (f, t)}

and pValueFieldType = parse {
    let! t = pType
    return ("", t)}

and pFieldType = attempt(pKeyValueFieldType) <|> pValueFieldType
and pFieldTypes = parse{
    let! fields = sepBy pFieldType (pstring "," .>> spaces)
    match fields with
    | [] -> return []
    | x -> return x |> List.mapi(fun i t -> addIndex i t)}


let rec pTerm = pAppTerm <|> pIf <|> pCaseOfCases <|> pLambda <|> pLet <|> pLetRec

and pCaseOfCases = parse {
    let! _ = pstring "case" .>> spaces1
    let! t = pTerm .>> spaces1
    let! _ = pstring "of" .>> spaces1
    let! cases = pCases
    return TmCase(t,cases)}

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


and pAppTerm = 
    attempt(pApp) <|> //TODO: ここおかしい？
    pFix <|> 
    pTimesFloat <|> 
    pSucc <|> 
    pPred <|> 
    pIsZero

and pApp = parse{
    let! items = many1 (pPathTerm)
    match items with
    | [] -> raise NoMatchError
    | x::[] -> return x
    | x::rest -> return Seq.fold (fun t1 t2 -> TmApp(t1, t2)) x rest}

and pFix = parse {
    let! _ = pstring "fix" .>> spaces
    let! path = pPathTerm
    return TmFix(path)}

and pTimesFloat = parse {
    let! _ = pstring "timesfloat" .>> spaces
    let! path1 = pPathTerm
    let! path2 = pPathTerm
    return TmTimesfloat(path1, path2)}
    
and pSucc = parse {
    let! _ = pstring "succ"
    do! spaces
    let! t =  pPathTerm
    return TmSucc(t)}

and pPred = parse {
    let! _ = pstring "pred"
    do! spaces
    let! t =  pPathTerm
    return TmPred(t)}

and pIsZero = parse {
    let! _ = pstring "iszero"
    do! spaces
    let! t =  pPathTerm
    return TmIsZero(t)}

and pATerm = 
    pParenTermSeq <|>
    pInert <|>
    pTrue <|>
    pFalse <|>
    pTmVar <|>
    pStringV <|>
    pUnit <|>
    pRecord <|>
    pIntV <|>
    pFloatV

    
and pAscribeTerm = attempt(parse {
    let! t = pATerm
    let! _ = pstring "as" .>> spaces1
    let! typ = pType
    return TmAscribe(t,typ)}) <|> pATerm

and pPathTerm = 
    attempt(pProj) <|> 
    pAscribeTerm

//TODO: シンプルに書き換える
and pint32_to_string = parse {
    let! i = pint32
    return i.ToString()}

// 循環してるのでこれはダメ
(*
and pProj = attempt(parse {
    let! path = pPathTerm
    let! _ = pstring "."
    let! i = pint32
    return TmProj(path, i.ToString())}) <|> parse {
    let! path = pPathTerm
    let! _ = pstring "."
    let! v = identifier
    return TmProj(path, v)}
*)
and pProj = parse {
    let! x = pAscribeTerm
    let! _ = pstring "."
    let! vs = sepBy (pint32_to_string <|> identifier) (pstring ".")
    return Seq.fold (fun x1 x2 -> TmProj(x1,x2)) x vs}



// これではaddnameが1つ多い。
//and pTermSeq = chainr1 (pTerm >>= fun t -> updateUserState(fun us -> addname "_" us) >>% t) (pstring ";" >>% fun x1 x2 -> TmApp(x1, TmAbs("_", TyUnit, x2)))
// TmAppするときにaddnameしたい。けどchainr1の中のfunでaddnameできない・・・
//and pTermSeq = chainr1 (pTerm) (pstring ";" >>% fun x1 x2 -> (fun t -> updateUserState(fun us -> addname "_" us)) >>% TmApp(x1, TmAbs("_", TyUnit, x2)))

// 循環しているので怒られる
(*
and pTermSeq = pTerm <|> pTermSeqX
and pTermSeqX = parse {
    let! t = pTerm
    let! _ = pstring ";"
    do! updateUserState(addname "_")
    let! ts = pTermSeq
    return TmApp(t, TmAbs("_", TyUnit, ts))}
*)

// 糖衣構文を使わない方法でパーサを書く
// 参考: http://hubfs.net/topic/None/60071
and pTermSeq : Parser<term,Context> = fun stream -> 
    let rep = chainr1 (pTerm) (pstring ";" >>% fun x1 x2 -> 
        stream.UserState <- (addname "_" stream.UserState) 
        TmApp(x1, TmAbs("_", TyUnit, x2))) stream
    Reply(rep.Result)
    

and pParenTermSeq = between (pstring "(") (pstring ")") pTermSeq

and pInert = parse {
    let! _ = pstring "inert" .>> spaces
    let! t = between (pstring "[") (pstring "]") pType
    return TmInert(t)}

and pTag = parse {
    let! _ = pstring "<" .>> spaces
    let! name = identifier
    let! _ = pstring "=" .>> spaces
    let! t = pTerm
    let! _ = pstring ">" .>> spaces1
    let! _ = pstring "as" .>> spaces1
    let! typ = pType
    return TmTag(name,t,typ)}

and pRecord = parse {
    let! fs = between (pstring "{") (pstring "}") pFields
    return TmRecord(fs)}

and pCase = parse {
    let! _ = pstring "<"
    let! v1 = identifier 
    let! _ = pstring "="
    let! v2 = identifier
    let! _ = pstring  ">" 
    let! _ = spaces >>. pstring "⇒" .>> spaces
    let! t1 = pAppTerm
    return (v1, (v2, t1))}

and pCases = sepBy pCase (pstring "|" .>> spaces)

and pKeyValueField = parse {
    let! f = identifier 
    let! _ = pstring "=" .>> spaces
    let! t = pTerm
    return (f, t)}

and pValueField = parse {
    let! t = pTerm
    return ("", t)}

and pField = attempt(pKeyValueField) <|> pValueField

and pFields = parse{
    let! fields = sepBy pField (pstring "," .>> spaces)
    match fields with
    | [] -> return []
    | x -> return x |> List.mapi(fun i t -> addIndex i t)}
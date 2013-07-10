[<AutoOpen>]
module Ast

type term =
    | TmTrue
    | TmFalse
    | TmIf of term * term * term
    | TmZero
    | TmSucc of term
    | TmPred of term
    | TmIsZero of term
    
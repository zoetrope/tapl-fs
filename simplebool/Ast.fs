[<AutoOpen>]
module Ast

type ty = 
    | TyArr of ty * ty
    | TyBool

type term =
    | TmVar of int * int
    | TmAbs of string * ty * term
    | TmApp of term * term
    | TmTrue
    | TmFalse
    | TmIf of term * term * term
    
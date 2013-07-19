[<AutoOpen>]
module Ast

type ty =
  | TyVar of int * int
  | TyId of string
  | TyArr of ty * ty
  | TyUnit
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyBool
  | TyString
  | TyFloat
  | TyNat

type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmCase of term * (string * (string * term)) list
  | TmTag of string * term * ty
  | TmVar of int * int
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLet of string * term * term
  | TmFix of term
  | TmString of string
  | TmUnit
  | TmAscribe of term * ty
  | TmRecord of (string * term) list
  | TmProj of term * string
  | TmFloat of float
  | TmTimesfloat of term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmInert of ty

type binding =
  | NameBind
  | TyVarBind
  | VarBind of ty
  | TmAbbBind of term * ty option
  | TyAbbBind of ty

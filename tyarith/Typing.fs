[<AutoOpen>]
module Typing

open FParsec

type ty = | TyBool | TyNat


exception TypeError of string

let rec typeof t =
  match t with
  | TmTrue -> TyBool
  | TmFalse -> TyBool
  | TmIf (t1, t2, t3) ->
      if (typeof t1) = TyBool
      then
        (let tyT2 = typeof t2
         in
           if tyT2 = (typeof t3)
           then tyT2
           else raise(TypeError("arms of conditional have different types")))
      else raise(TypeError("guard of conditional not a boolean"))
  | TmZero -> TyNat
  | TmSucc t1 ->
      if (typeof t1) = TyNat
      then TyNat
      else raise(TypeError("argument of succ is not a number"))
  | TmPred t1 ->
      if (typeof t1) = TyNat
      then TyNat
      else raise(TypeError("argument of pred is not a number"))
  | TmIsZero t1 ->
      if (typeof t1) = TyNat
      then TyBool
      else raise(TypeError("argument of iszero is not a number"))
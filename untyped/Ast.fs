[<AutoOpen>]
module Ast


type term =
  | TmVar of int * int      // 変数        ：index * length
  | TmAbs of string * term  // ラムダ抽象  ：変数名 * term
  | TmApp of term * term    // 関数適用    ：
    
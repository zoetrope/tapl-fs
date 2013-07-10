[<AutoOpen>]
module Core

exception NoRuleApplies

let rec isnumericval = function
    | TmZero -> true
    | TmSucc t1 -> isnumericval t1
    | _ -> false

let rec isval = function
    | TmTrue _ -> true
    | TmFalse _ -> true
    | t when isnumericval t -> true
    | _ -> false

let rec eval1 = function
    | TmIf (TmTrue _, t2, t3) -> t2

    | TmIf (TmFalse _, t2, t3) -> t3

    | TmIf (t1, t2, t3) ->
        let t1' = eval1 t1
        TmIf (t1', t2, t3)

    | TmSucc (t1) ->
        let t1' = eval1 t1
        TmSucc (t1')

    | TmPred (TmZero _) ->
        TmZero

    | TmPred (TmSucc nv1) when isnumericval nv1 ->
        nv1

    | TmPred (t1) ->
        let t1' = eval1 t1
        TmPred (t1')

    | TmIsZero (TmZero _) ->
        TmTrue

    | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
        TmFalse

    | TmIsZero (t1) ->
        let t1' = eval1 t1
        TmIsZero (t1')

    | _ -> 
        raise NoRuleApplies

let rec eval t =
    try
        let t' = eval1 t
        eval t'
    with
    | NoRuleApplies -> t
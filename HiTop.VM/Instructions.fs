﻿module HiTop.VM.Instructions

open System.Collections.Generic
open HiTop.VM.CoreTypes
open HiTop.VM.Engine

let private make name op =
    { ShortName = name; Op = op }

let private arith2 name f = make name (fun engine ->
    let stack = engine.Stack

    if engine |> Engine.willHalt then engine
    else

    let a = stack |> Stack.peekAt 0
    let b = stack |> Stack.peekAt 1

    match (a, b) with
    | Some(Value a), Some(Value b) ->
        stack |> Stack.dropn 2
        stack |> Stack.push (Value(f a b))
        engine

    | _, _ ->
        engine // nop
)

module Arithmetic =
    let add = arith2 "add" (+)

    let sub = arith2 "sub" (-)

    let mul = arith2 "mul" (*)

    let div = arith2 "div" (fun a -> function
                            | 0uy -> 0uy
                            | b   -> a / b)

    let ``mod`` = arith2 "mod" (%)

    let all =
        [add; sub; mul; div; ``mod``;]

let all = Arithmetic.all
﻿module HiTop.VM.CoreTypes

open System.Collections.Generic

type BinaryReader = System.IO.BinaryReader

type UnbuiltInstructionSet = Instruction list

and BuiltInstructionSet = IReadOnlyDictionary<byte, Instruction>

and Operation = Engine -> Engine

and Instruction = {
    ShortName: string;
    Op: Operation
}


and StackElement =
    | Value of byte
    | Operation of Operation
    | Lambda of Operation

and Engine = {
    NextReadAddress: int64
    IsHalted: bool
    InstructionSet: BuiltInstructionSet
    Stack: List<StackElement>
    Program: BinaryReader
}

let equalAsValue (a: StackElement) (b: StackElement) =
    match (a, b) with
    | Value(x), Value(y) -> x = y
    | _ -> false

let equalAsOptValue (a: StackElement) (b: StackElement option) =
    match (a, b) with
    | Value(x), Some(Value(y)) -> x = y
    | _ -> false
module HiTop.VM.CoreTypes

open System.Collections.Generic
open System.IO

type Result<'TSuccess,'TFailure> = 
     | Success of 'TSuccess
     | Failure of 'TFailure

type UnbuiltInstructionSet = Instruction list

and BuiltInstructionSet = IReadOnlyDictionary<byte, Instruction>

and Operation = Engine -> Engine

and Instruction = {
    ShortName: string;
    Op: Operation
}

and Lambda = Engine -> Engine option

and StackElement =
    | Value of byte
    | Operation of Operation
    | Lambda of Lambda

and Engine = {
    IsHalted: bool
    NextReadAddress: int64
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
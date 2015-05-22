[<AutoOpen>]
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

and LambdaArg =
    | StackElement of StackElement
    | Placeholder

and LambdaState = {
    ShortName: string
    Args: LambdaArg list
    Lambda: Lambda
}

and StackElement =
    | Value of byte
    | Instruction of Instruction
    | Lambda of LambdaState

and Stack = ResizeArray<StackElement>

and Output =
    | Byte of byte
    | Buffer of byte array

and Engine = {
    IsHalted: bool
    Cycles: uint64
    NextReadAddress: int64
    InstructionSet: BuiltInstructionSet
    Stack: Stack
    Program: BinaryReader
    LastOutput: Output option
}


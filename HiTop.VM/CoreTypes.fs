[<AutoOpen>]
module HiTop.VM.CoreTypes

open System.Collections.Generic
open System.IO

[<AutoOpen>]
module Literals =
    [<Literal>]
    let hitop_false = 0uy

    [<Literal>]
    let hitop_true = 1uy

type Result<'TSuccess,'TFailure> = 
     | Success of 'TSuccess
     | Failure of 'TFailure

type UnbuiltInstructionSet = ByteCode list

and BuiltInstructionSet = IReadOnlyDictionary<byte, ByteCode>

and Operation = Engine -> Engine option

and ByteCode =
    | Instruction of Instruction
    | EncodedByteMarker
    | LoopBeginMarker
    | LoopEndMarker

and Instruction = {
    ShortName: string;
    Op: Operation
}

and StackElement = byte

and Stack = ResizeArray<StackElement>

and Output =
    | Byte of byte
    | Buffer of byte array

and StepResult =
    | PushedUnencodedByte of byte
    | PushedEncodedByte of byte
    | ExecutedOperation of string
    | PushedFailedOperation of string
    | ContinuedAfterLoopBeginMarker
    | ContinuedAfterLoopEndMarker
    | JumpedToLoopBeginMarker
    | JumpedToLoopEndMarker

and Engine = {
    IsHalted: bool
    Cycles: uint64
    InstructionSet: BuiltInstructionSet
    Stack: Stack
    Program: BinaryReader
    LastOutput: Output option
    LastOperation: StepResult option
}


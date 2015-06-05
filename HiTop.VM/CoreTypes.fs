[<AutoOpen>]
module HiTop.VM.CoreTypes

open System.Collections.Generic
open System.IO

type Result<'TSuccess,'TFailure> = 
     | Success of 'TSuccess
     | Failure of 'TFailure

type UnbuiltInstructionSet = ByteCode list

and BuiltInstructionSet = IReadOnlyDictionary<byte, ByteCode>

and Operation = Engine -> Engine option

and ByteCode =
    | Instruction of Instruction
    | EncodedByteMarker

and Instruction = {
    ShortName: string;
    Op: Operation
}

and StackElement = byte

and Stack = ResizeArray<StackElement>

and Output =
    | Byte of byte
    | Buffer of byte array

and LastOperation =
    | PushedUnencodedByte of byte
    | PushedEncodedByte of byte
    | ExecutedOperation of string
    | PushedFailedOperation of string

and Engine = {
    IsHalted: bool
    Cycles: uint64
    NextReadAddress: int64
    InstructionSet: BuiltInstructionSet
    Stack: Stack
    Program: BinaryReader
    LastOutput: Output option
    LastOperation: LastOperation option
}


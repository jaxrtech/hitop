[<AutoOpen>]
module HiTop.VM.CoreTypes

open System.IO

[<AutoOpen>]
module private Helpers =
    let equalsBy f x (yobj: obj) =
        match yobj with
        | :? 'T as y -> (f x = f y)
        | _ -> false
 
    let hashWith f x = hash (f x)
 
    let compareBy f x (yobj: obj) =
        match yobj with
        | :? 'T as y -> compare (f x) (f y)
        | _ -> invalidArg "yobj" "cannot compare values of different types"

[<AutoOpen>]
module Literals =
    [<Literal>]
    let hitop_false = 0uy

    [<Literal>]
    let hitop_true = 1uy

type Result<'TSuccess,'TFailure> = 
     | Success of 'TSuccess
     | Failure of 'TFailure

and UnbuiltInstructionSet = Map<byte, ByteCode>

and BuiltInstructionSet = {
    FromByte: Map<byte, ByteCode>
    FromByteCode: Map<ByteCode, byte>
}

and Operation = Engine -> Engine option

and ByteCode =
    | Value of byte
    | Instruction of Instruction
    | EncodedByteMarker
    | LoopBeginMarker
    | LoopEndMarker

// Since we can assume that all the instructions are unique, we can use the name to support equality
// and comparisons to make life easier. This could be possible dangerous though...
and [<CustomEquality; CustomComparison>]
    Instruction =
        { ShortName: string;
          Op: Operation }

        static member private _ShortName(x) = x.ShortName

        override x.Equals y = equalsBy Instruction._ShortName x y
        override x.GetHashCode() = hashWith Instruction._ShortName x

        interface System.IComparable with
            member x.CompareTo y = compareBy Instruction._ShortName x y

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


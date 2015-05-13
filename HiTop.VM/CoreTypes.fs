module HiTop.VM.CoreTypes

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
    | Operator of Operation
    // TODO: | Lambda of Lambda

and Engine = {
    NextReadAddress: int64;
    IsHalted: bool;
    InstructionSet: BuiltInstructionSet
    Stack: List<StackElement>
    Program: BinaryReader
}

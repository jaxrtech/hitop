module HiTop.VM.Engine

open System.Collections.Generic
open HiTop.VM.CoreTypes

let createFromStream stream instructionSet =
    { NextReadAddress = 0L;
      IsHalted = false;
      InstructionSet = instructionSet
      Stack = List<StackElement>();
      Program = new BinaryReader(stream) }

let isEndOfProgram (engine: Engine) : bool =
    engine.NextReadAddress = engine.Program.BaseStream.Length

let step (engine: Engine) : Engine =
    if engine.IsHalted then engine
    else

    if isEndOfProgram engine then
        { engine with IsHalted = true }
    else

    let raw = engine.Program.ReadByte()

    failwith "Not implemented (yet)"
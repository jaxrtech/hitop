﻿module HiTop.VM.Engine

open System.Collections.Generic
open System.IO
open HiTop.VM.CoreTypes

let createFromStream stream instructionSet =
    { NextReadAddress = 0L;
      InstructionSet = instructionSet
      Stack = List<StackElement>();
      Program = new BinaryReader(stream) }

let createFromBuffer (buffer: byte array) instructionSet =
    let stream = new MemoryStream(buffer)
    let reader = new BinaryReader(stream)

    createFromStream stream instructionSet

let isEndOfProgram (engine: Engine) : bool =
    engine.NextReadAddress = engine.Program.BaseStream.Length

// Allowing for further expansion
let willHalt = isEndOfProgram

let step (engine: Engine) : Engine =
    if willHalt engine then engine
    else

    let raw = engine.Program.ReadByte()

    // If the byte is not assigned, it is a raw encoded byte
    let engine' =
        if not (engine.InstructionSet.ContainsKey(raw)) then
            engine.Stack |> Stack.push (Value(raw))
            engine
        else
            engine.InstructionSet.[raw].Op engine

    { engine' with NextReadAddress = engine.NextReadAddress + 1L }
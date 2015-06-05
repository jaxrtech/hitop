module HiTop.VM.Engine

open System.IO
open HiTop.VM.CoreTypes
open HiTop.VM.Stack

let createFromStream stream instructionSet =
    { IsHalted = false
      Cycles = 0UL
      NextReadAddress = 0L
      InstructionSet = instructionSet
      Stack = Stack.create ()
      Program = new BinaryReader(stream)
      LastOutput = None
      LastOperation = None }

let createFromBuffer (buffer: byte array) instructionSet =
    let stream = new MemoryStream(buffer)
    let reader = new BinaryReader(stream)

    createFromStream stream instructionSet

let dispose (engine: Engine) =
    engine.Program.Close()

let isEndOfProgram (engine: Engine) : bool =
    engine.NextReadAddress = engine.Program.BaseStream.Length

let private incrementCyles (engine: Engine) =
    { engine with Cycles = engine.Cycles + 1UL }

let private incrementNextReadAddress (engine: Engine) =
    { engine with NextReadAddress = engine.NextReadAddress + 1L }

let private withLastOperation (op: LastOperation) (engine: Engine) =
    { engine with LastOperation = Some(op) }

let private withNoLastOperation (engine: Engine) =
    { engine with LastOperation = None}

let private withNoLastOutput (engine: Engine) =
    { engine with LastOutput = None }

let private markAsHalted (engine: Engine) =
    { engine with IsHalted = true }

[<AutoOpen>]
module private Check =
    let check f x =
        match f x with
        | Some(x) -> Some(x)
        | None -> None

    let checkUnit f = check f ()

    let checkWith f last =
        match last with
        | Some(x) -> Some(x)
        | None -> checkUnit f

    let checkOrElse f last =
        match last with
        | Some(x) -> x
        | None -> f ()

let step (engine: Engine) : Engine =
    let push engine value =
        engine.Stack |> StrictStack.push value
        engine

    let checkForEndOfProgram engine =
        if isEndOfProgram engine then
            engine
            |> markAsHalted
            |> withNoLastOperation
            |> Some
        else
            None
    
    // Make sure to clear the last output
    let engine = engine |> withNoLastOutput

    check checkForEndOfProgram engine
    |> checkOrElse (fun () ->
        let raw = engine.Program.ReadByte()

        // If the byte is not assigned, it is a raw unencoded byte
        if not (engine.InstructionSet.ContainsKey(raw)) then
            raw
            |> push engine
            |> withLastOperation (PushedUnencodedByte(raw))
        else
            let op = engine.InstructionSet.[raw]
            match op with
            | EncodedByteMarker ->
                // Try to read the next byte as a raw byte
                let engine' = engine |> incrementNextReadAddress

                check checkForEndOfProgram engine'
                |> checkOrElse (fun () -> 
                    let raw' = engine.Program.ReadByte()
                    
                    raw'
                    |> push engine'
                    |> withLastOperation (PushedEncodedByte(raw')))

            | Instruction op ->
                let result = op.Op engine
                match result with
                | Some(engine') ->
                    engine'
                    |> withLastOperation (ExecutedOperation(op.ShortName))

                | None ->
                    // The operation does not have sufficient arguments and will just be converted
                    // to the raw byte value and pushed on the stack
                    raw
                    |> push engine
                    |> withLastOperation (PushedFailedOperation(op.ShortName)))

    |> incrementCyles
    |> incrementNextReadAddress
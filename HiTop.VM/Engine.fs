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
      LastOutput = None }

let createFromBuffer (buffer: byte array) instructionSet =
    let stream = new MemoryStream(buffer)
    let reader = new BinaryReader(stream)

    createFromStream stream instructionSet

let isEndOfProgram (engine: Engine) : bool =
    engine.NextReadAddress = engine.Program.BaseStream.Length

let private incrementCyles (engine: Engine) =
    { engine with Cycles = engine.Cycles + 1UL }

let step (engine: Engine) : Engine =
    let check f =
        match f () with
        | Some(x) -> Some(x)
        | None -> None

    let checkWith f last =
        match last with
        | Some(x) -> Some(x)
        | None -> check f

    let checkOrElse f last =
        match last with
        | Some(x) -> x
        | None -> f ()

    let peek () = engine.Stack |> StrictStack.peek
    let peekHook () = engine.Stack |> StrictStack.peekHook
    let peekAt i = engine.Stack |> StrictStack.peekAt i
    let peekAtHook i = engine.Stack |> StrictStack.peekAtHook i

    //

    let checkForInstruction () =
        let x, pop = peekHook ()
        match x with
        | Some(Instruction x) ->
            pop ()
            Some(x.Op engine)

        | _ -> None

    // Check first if we have a lambda that can be applied to
    let checkForLambda () =
        // If you think as if the stack's is the right most element
        match (peekAt 1, peekAt 0) with
        | Some(Lambda f), Some(x) ->
            f.Lambda engine
        
        | _, _ -> None

    let checkForEndOfProgram () =
        if isEndOfProgram engine then
            Some({ engine with IsHalted = true })
        else
            None

    // Only check if we should halt after seeing if anything on the stack can be evaluated

    check checkForInstruction
    |> checkWith checkForLambda
    |> checkWith checkForEndOfProgram
    |> checkOrElse (fun () ->

        let raw = engine.Program.ReadByte()

        // If the byte is not assigned, it is a raw encoded byte
        let engine' =
            if not (engine.InstructionSet.ContainsKey(raw)) then
                engine.Stack |> StrictStack.push (Value(raw))
                engine
            else
                let op = engine.InstructionSet.[raw]
                engine.Stack |> StrictStack.push (Instruction(op))
                engine

        { engine' with NextReadAddress = engine.NextReadAddress + 1L })

    |> incrementCyles
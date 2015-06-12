﻿module HiTop.VM.Runner.Program

open HiTop.VM
open HiTop.VM.InstructionSet

type StepResult =
     | Halted of Engine
     | Stepped of int * Engine

let step (i, engine) =
    let printStack i engine =
        printfn "[%d] S> %s | [%A]" i (Stack.toString engine.Stack) (engine.LastOperation)
        printfn "[%d] D> %A" i engine.LastOutput
        i + 1

    let printStackIfRunning i engine =
        // Only print the state out again if we are not halted
        if engine.IsHalted then
            printfn "     >> Done"
            i
        else
            printStack i engine

    let evalPrintWrapper f =
        match i with
        | 0 ->
            let i = printStack i engine
            let engine' = f ()
            let i' = printStackIfRunning i engine'
            (engine', i')

        | _ ->
            let engine' = f ()
            let i' = printStackIfRunning i engine'
            (engine', i')

    let step () = engine |> Engine.step
    let engine', i' = evalPrintWrapper step

    if engine'.IsHalted then
        Halted(engine')
    else
        Stepped(i', engine')

let eval engine =
    let rec f buffer engine result =
        let appendToBuffer output =
             output |> Output.appendTo buffer

        match result with
        | Halted engine' ->
            let buffer' = appendToBuffer engine'.LastOutput
            (engine', buffer')

        | Stepped(i, engine') ->
            let buffer' = appendToBuffer engine'.LastOutput
            f buffer' engine' (step (i, engine'))

    f (Array.zeroCreate<byte> 0) engine (step (0, engine))

[<EntryPoint>]
let main argv = 
    let instructionSet = InstructionSet.build Instructions.all

    let bytecode =
        let random = new System.Random()

        let nextByte () =
            // Try to even out the distribution of bytes and operations generated
            match random.NextDouble() with
            | x when x < 0.40 -> // 40% of the time: generate a raw byte
                let n = random.Next(0, System.Byte.MaxValue |> int)
                n |> byte
                
            | x -> // 60% of the time: generate an operation
                let i = random.Next(0, instructionSet.Count)
                Seq.nth i instructionSet.Keys
        
        Array.init 25 (fun _ -> nextByte ())

    let engine =
        Engine.create instructionSet
        |> Engine.loadFromBuffer bytecode

    eval engine |> ignore
        
    System.Console.ReadLine() |> ignore
    0

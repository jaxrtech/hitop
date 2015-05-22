module HiTop.Program

open HiTop.VM
open HiTop.VM.InstructionSet

type StepResult =
     | Halted of Engine
     | Stepped of int * Engine

let step (i, engine) =
    let printStack i engine =
        printfn "[%d] S> %s" i (Stack.toString engine.Stack)
        printfn "[%d] D> %A" i engine.LastOutput
        i + 1

    let evalPrintWrapper f =
        match i with
        | 0 ->
            let i = printStack i engine
            let engine' = f ()
            let i = printStack i engine'
            (engine', i)
        | _ ->
            let engine' = f ()
            let i = printStack i engine'
            (engine', i)

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
    let instructionSet =
        let x = InstructionSet.build Instructions.all
        match x with
        | Failure TooManyInstructions ->
            failwith "error: too many instructions in instruction set. number of instructions exceeds 256."
        | Success x -> x

    let bytecode =
        let random = new System.Random()
        let x = Array.zeroCreate 25
        random.NextBytes(x)
        x

    let engine = Engine.createFromBuffer bytecode instructionSet
    eval engine |> ignore
        
    printfn "   > Done"
    System.Console.ReadLine() |> ignore

    0

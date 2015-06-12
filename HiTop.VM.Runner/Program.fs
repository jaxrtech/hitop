module HiTop.VM.Runner.Program

open HiTop.VM

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
    let instructionSet =
        InstructionSet.filledAtTop Instructions.all
        |> InstructionSet.build

    let bytecode =
        let pregen = Helpers.prepareRandomByte instructionSet
        Array.init 25 (Helpers.initWithRandomByte pregen)

    let engine =
        Engine.initFromBuffer instructionSet bytecode

    eval engine |> ignore
        
    System.Console.ReadLine() |> ignore
    0

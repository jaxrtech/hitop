module HiTop.Program

open HiTop.VM
open HiTop.VM.InstructionSet

let eval engine : Engine =
    let rec f i (engine: Engine) =
        let printStack i =
            printfn "[%d]> %s" i (Stack.toString engine.Stack)
            i + 1

        let evalPrintWrapper f =
            match i with
            | 0 ->
                let i = printStack i
                let x = f ()
                let i = printStack i
                (x, i)
            | _ ->
                let x = f ()
                let i = printStack i
                (x, i)

        let step () = engine |> Engine.step
        let engine', i' = evalPrintWrapper step

        if engine'.IsHalted then
            engine'
        else
            f i' engine'

    f 0 engine

[<EntryPoint>]
let main argv = 
    let instructionSet =
        let x = InstructionSet.build Instructions.all
        match x with
        | Failure TooManyInstructions ->
            failwith "error: too many instructions in instruction set. number of instructions exceeds 256."
        | Success x -> x

    let random = new System.Random()
    let buffer = Array.zeroCreate 50
    random.NextBytes(buffer)

    let engine = Engine.createFromBuffer buffer instructionSet
    
    eval engine |> ignore

    printfn "   > Done"
    System.Console.ReadLine() |> ignore

    0

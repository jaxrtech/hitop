module HiTop.Program

open HiTop.VM
open HiTop.VM.CoreTypes
open HiTop.VM.InstructionSet

let eval engine =
    let rec f i (engine: Engine) =
        if engine |> Engine.willHalt then engine
        else

        let printStack i =
            printfn "[%d]> %A" i (engine.Stack)
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

    let buffer = [| 1uy; 2uy; 3uy; 250uy; |]
    let engine = Engine.createFromBuffer buffer instructionSet
    
    eval engine |> ignore
    System.Console.ReadLine() |> ignore

    0

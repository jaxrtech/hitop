module HiTop.Program

open HiTop.VM
open HiTop.VM.CoreTypes
open HiTop.VM.InstructionSet

let eval engine =
    let rec f i (engine: Engine) =
        if engine.IsHalted then engine
        else

        let printStack () =
            printfn "[%d]> %A" (i) (engine.Stack)

        let evalPrintWrapper f =
            match i with
            | 0 ->
                printStack ()
                let x = f ()
                printStack ()
                x
            | _ ->
                let x = f ()
                printStack ()
                x

        let step () = engine |> Engine.step
        let engine' = evalPrintWrapper step

        f (i + 1) engine'

    f 0 engine

[<EntryPoint>]
let main argv = 
    let instructionSet =
        let x = InstructionSet.build Instructions.all
        match x with
        | Failure TooManyInstructions ->
            failwith "error: too many instructions. number of instructions exceeds 256."
        | Success x -> x

    let buffer = [| 1uy; 2uy; 3uy; 250uy; |]
    let engine = Engine.createFromBuffer buffer instructionSet
    
    eval engine |> ignore
    System.Console.ReadLine() |> ignore

    0

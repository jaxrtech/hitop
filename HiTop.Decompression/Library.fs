module HiTop.Decompression

open System.IO
open HiTop.VM

type ProgramContext = {
    Program: byte []
    InstructionSet: BuiltInstructionSet
    OutputLength: int
}

let outputToStreamWithProgress (context: ProgramContext) (output: Stream) (onProgress: int -> unit) =
    let engine = 
        Engine.initFromBuffer context.InstructionSet context.Program
    
    let rec loop outputCount engine' =
        onProgress outputCount

        let engine' = engine |> Engine.step
            
        let length = context.OutputLength

        let willHalt =
            engine'.IsHalted
            || outputCount >= context.OutputLength

        if willHalt then
            onProgress outputCount
            ()
        else
            match engine'.LastOutput with
            | None ->
                loop outputCount engine'
                
            | Some(Byte x) ->
                output.WriteByte(x)
                loop (outputCount + 1) engine'

            | Some (Buffer buffer) ->
                // Ensure that the array lengths are always the same by making sure that the
                // remaining length in the target stream takes precedent over what the program
                // is returning to us

                let remainingLength = length - (output.Position |> int)

                // Truncate `buffer` if necessary
                let buffer' =
                    if Array.length buffer > remainingLength then
                        Array.init remainingLength (Array.get buffer)
                    else
                        buffer

                output.Write(buffer, 0, buffer.Length)

                loop (outputCount + length) engine'

    loop 0 engine
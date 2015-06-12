module HiTop.Decompression.Runner.Program

open System.IO
open HiTop
open HiTop.VM
open HiTop.Decompression
open HiTop.Serialization
open HiTop.Decompression.Runner

let printHeader () =
    printfn "HiTop Decompressor indev"
    printfn "Copyright (c) 2015 Josh Bowden & Patrick Ryan"
    printfn "CONFIDENTIAL MATERIAL. DO NOT REDISTRIBUTE.\n"
    printfn "info: started at %s" (System.DateTime.Now.ToString())

[<EntryPoint>]
let main argv =
    let result, args = parseArgs argv

    match result with
    | UsageRequested ->
        args.Usage() |> printfn "%s"
        0
    
    | Settings { InputPath = inputPath; OutputPath = outputPath } ->

    let input = File.OpenRead(inputPath)

    let result = Serializer.derserializeFrom input
    match result with
    | Failure BadMagicSignature ->
        failwith "error: unable to read archive. bad magic signature."

    | Failure BadFileName ->
        failwith "error: unable to read archive. unable to read file name."

    | Failure BadFileSize ->
        failwith "error: unable to read archive. invalid file size specified."

    | Failure UnpectedEndOfFile ->
        failwith "error: unable to read archive. end of file was unexpected."

    | Success fileContext ->

    let decompressionContext =
        // TODO: We are assuming we are using the full instruction set
        { ProgramContext.Program = fileContext.Program
          OutputLength = fileContext.FileSize
          InstructionSet =
             InstructionSet.filledAtTop Instructions.all
             |> InstructionSet.build }

    let outputPath' =
        match outputPath with
        | Some x -> x
        | None -> fileContext.FileName

    let output = File.OpenWrite(outputPath')

    Decompression.outputToStreamWithProgress decompressionContext output (fun count ->
        let percent = (count |> float) / (decompressionContext.OutputLength |> float) * 100.0
        printfn "%.0f%s" percent "%")

    0
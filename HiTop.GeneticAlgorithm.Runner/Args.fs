[<AutoOpen>]
module HiTop.GeneticAlgorithm.Runner.Args

open System.IO
open Nessos.UnionArgParser

type CommandLineArgs =
     | [<Mandatory; AltCommandLine("-i")>] Input of string
     | [<AltCommandLine("-o")>] Output of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "specify the path of the input file to compress."
            | Output _ -> "specify the path of the output file to save the resulting miniprogram to."

let private parser = UnionArgParser.Create<CommandLineArgs>()

//

type AppSettings = {
    InputPath: string
    OutputPath: string
}

type ParseArgsResult =
     | UsageRequested
     | Settings of AppSettings 

let parseArgs argv =
    let args = parser.Parse argv

    if args.IsUsageRequested then
        (UsageRequested, args)
    else

    let ensurePath name path = 
        if path |> File.Exists |> not then 
            failwith (sprintf "%s file does not exist." name)
        else path
 
    let inputPath = args.PostProcessResult (<@ Input @>, ensurePath "input")

    let outputPath =
        if args.Contains <@ Output @> then
            args.PostProcessResult(<@ Output @>, ensurePath "output")
        else
            let name = Path.GetFileNameWithoutExtension(inputPath)
            name + ".hitop"

    let settings =
        { InputPath = inputPath
          OutputPath = outputPath }
        |> Settings

    (settings, args)
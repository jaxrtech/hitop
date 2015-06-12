module HiTop.GeneticAlgorithm.Runner.Program

open System.IO
open HiTop.VM
open HiTop.Serialization
open HiTop.GeneticAlgorithm
open HiTop.GeneticAlgorithm.Runner

let printHeader () =
    printfn "HiTop Compressor indev"
    printfn "Copyright (c) 2015 Josh Bowden & Patrick Ryan"
    printfn "CONFIDENTIAL MATERIAL. DO NOT REDISTRIBUTE.\n"
    printfn "info: started at %s" (System.DateTime.Now.ToString())

let evolveUntilOptimal evaluationSettings initialPopulation =
    let rec loop gen population =
        let evaluatedPopulation =
            population
            |> Population.evaluate evaluationSettings

        let best =
            evaluatedPopulation
            |> Array.maxBy snd

        let bestFitness =
            snd best

        let worstFitness =
            let worst =
                evaluatedPopulation
                |> Array.minBy snd

            snd worst

        let avgFitness =
            let total =
                evaluatedPopulation
                |> Array.sumBy snd

            total / (evaluatedPopulation.Length |> float)

        printfn "gen %d: best fitness %.4f | avg fitness %.4f | worst fitness %.4f" gen bestFitness avgFitness worstFitness

        if Organism.isOptimalFitness evaluationSettings bestFitness then
            printfn "[!] optimal fitness reached in gen %d" gen
            best
        else

        let nextPopulation =
            evaluatedPopulation
            |> Population.select
            |> Population.reproduce

        loop (gen + 1) nextPopulation

    loop 0 initialPopulation

[<EntryPoint>]
let main argv = 

    let result, args = parseArgs argv

    match result with
    | UsageRequested ->
        args.Usage() |> printfn "%s"
        0
    
    | Settings { InputPath = inputPath; OutputPath = outputPath } ->

    let target = File.OpenRead(inputPath)

    let instructionSet =
        InstructionSet.filledAtTop Instructions.all
        |> InstructionSet.build

    let targetLength = target.Length

    let populationSettings = {
        PopulationCount = 25
        ProgramLength = targetLength |> int32
        InstructionSet = instructionSet
    }

    let evaluationSettings =
        { Target = new BinaryReader(target)
          TargetLength = targetLength
          InstructionSet = instructionSet }

    printHeader ()

    let initialPopulation =
        printfn "info: creating gen 0..."

        let x = Population.create populationSettings

        printfn "info: finished"
        x

    let organism, _ = initialPopulation|> evolveUntilOptimal evaluationSettings

    let context = {
        SerializationContext.Program = organism
        FileName = Path.GetFileName(inputPath)
        FileSize = targetLength |> int32
    }

    let output = File.OpenWrite(outputPath)

    context |> Serializer.serializeTo output

    0

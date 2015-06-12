open HiTop.GeneticAlgorithm.Runner

open System.IO
open HiTop.VM
open HiTop.GeneticAlgorithm

let printHeader () =
    printfn "HiTop Compression indev"
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

    let target = File.OpenRead(inputPath)

    let instructionSet =
        InstructionSet.filledAtTop Instructions.all
        |> InstructionSet.build

    let populationSettings = {
        PopulationCount = 25
        ProgramLength = target.Length |> int32
        InstructionSet = instructionSet
    }

    let evaluationSettings =
        { Target = new BinaryReader(target)
          TargetLength = target.Length
          InstructionSet = instructionSet }

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

    printHeader ()

    printfn "info: creating gen 0..."

    let initialPopulation = Population.create populationSettings

    printfn "info: finished"

    let organism, _ = loop 0 initialPopulation

    let check = Organism.evaluate evaluationSettings organism
    assert (check |> Organism.isOptimalFitness evaluationSettings)

    File.WriteAllBytes(outputPath, organism)

    0

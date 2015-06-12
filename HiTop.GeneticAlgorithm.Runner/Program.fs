open HiTop.GeneticAlgorithm

open System.IO
open HiTop.VM

let printHeader () =
    printfn "HiTop Compression indev"
    printfn "Copyright (c) 2015 Josh Bowden & Patrick Ryan"
    printfn "CONFIDENTIAL MATERIAL. DO NOT REDISTRIBUTE.\n"
    printfn "info: started at %s" (System.DateTime.Now.ToString())

[<EntryPoint>]
let main argv = 
    
    let target = File.OpenRead("test_mini.txt") 

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

        let bestFitness = snd best

        printfn "gen %d: best fitness %.2f" gen bestFitness

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

    let initialPopulation = Population.create populationSettings

    let (organism, fitness) = loop 0 initialPopulation

    let check = Organism.evaluate evaluationSettings organism

    System.Console.ReadLine() |> ignore
    0

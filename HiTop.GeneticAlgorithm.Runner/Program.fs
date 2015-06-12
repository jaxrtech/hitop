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

    let (organism, fitness) = loop 0 initialPopulation

    let check = Organism.evaluate evaluationSettings organism

    System.Console.ReadLine() |> ignore
    0

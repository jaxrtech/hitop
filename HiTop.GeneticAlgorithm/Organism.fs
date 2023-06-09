﻿module HiTop.GeneticAlgorithm.Organism

open HiTop.VM

let createRandom programLength instructionSet : OrganismT =
    Array.init programLength (Helpers.initWithRandomByte instructionSet)

let private score (settings: EvaluationSettings) engine =
    let target = settings.Target
    target.BaseStream.Position <- 0L

    let rec score acc outputCount engine =
        let engine' = engine |> Engine.step
            
        let length = settings.TargetLength
        let lengthf = length |> float

        let willStop =
            let isHalted = engine'.IsHalted
            let exceededMaxCycles = engine'.Cycles > (1e7 |> uint64)
            
            if exceededMaxCycles then
                printfn "debug: max cycles exceeded"
            else ()

            let exceededMaxStackSize = engine'.Stack.Count > ((lengthf * 4.0) |> int)
            
            if exceededMaxStackSize then
                printfn "debug: max stack size reached"
            else ()

            let exceededMaxOutput = (outputCount |> int64) >= length

            if exceededMaxOutput then
                printfn "debug: max output length reached"
            else ()

            isHalted
            || exceededMaxCycles
            || exceededMaxStackSize
            || exceededMaxOutput

        if willStop then
            (acc, outputCount)
        else
            match engine'.LastOutput with
            | None ->
                score acc outputCount engine'
                
            | Some(Byte x) ->
                if x = target.ReadByte() then
                    score (acc + 1UL) (outputCount + 1UL) engine'
                else
                    score acc (outputCount + 1UL) engine'
            | Some (Buffer buffer) ->
                // Ensure that the array lengths are always the same by making sure that the
                // remaining length in the target stream takes precedent over what the program
                // is returning to us

                let remainingLength =
                    let x = length - target.BaseStream.Position
                        
                    // HACK: F#'s `Array` module uses `int32`. Will need to fix this to handle
                    //       larger files
                    assert (x <= (System.Int32.MaxValue |> int64))

                    x |> int32

                // Truncate `buffer` if necessary
                let buffer' =
                    if Array.length buffer > remainingLength then
                        Array.init remainingLength (Array.get buffer)
                    else
                        buffer

                // At this point, `buffer'` is the correct length and we should get the same
                // length filled from the expected stream
                let length = Array.length buffer'
                let expected = target.ReadBytes(length)

                assert (length = Array.length expected)

                let points =
                    expected
                    |> Array.zip buffer'
                    |> Array.map (fun (exp, res) -> exp = res)
                    |> Array.map (function
                                    | true -> 1
                                    | false -> 0)
                    |> Array.sum
                    |> uint64

                score (acc + points) (outputCount + (length |> uint64)) engine'

    let correct, output = score 0UL 0UL engine
    correct + output

let evaluate (settings: EvaluationSettings) (organism: OrganismT) : Fitness =
    let program = organism

    let engine' =
        Engine.initFromBuffer settings.InstructionSet program

    // Get the raw score that being (number output + number correct)
    let raw = engine' |> score settings |> float

    // -1.0 -> no output, none correct
    //  0.0 -> full output, none correct
    // +1.0 -> full output, all correct

    // Make sure to `* 2.0` since a perfect `raw` score is 2 times the length of the target 
    let max = (settings.TargetLength |> float) * 2.0

    raw / max * 2.0 - 1.0

let isOptimalFitness (settings: EvaluationSettings) (fitness: Fitness) : bool =
    fitness >= 1.0

let toEvaluated (settings: EvaluationSettings) (organism: OrganismT) : EvaluatedOrganism =
    (organism, organism |> evaluate settings)

let crossover (parents: OrganismPair) : OrganismT =
    let parentA, parentB = parents

    // Check if we even need to do crossover if we are trying to save an "elite"
    if parentA = parentB then
        parentA
    else
        
    // Assume they are equal length for now
    assert (parentA.Length = parentB.Length)

    let length = parentA.Length

    let midpoint = random.Next(0, length)

    let getSource i =
        if random.NextDouble() < 0.50 then
            parentA
        else
            parentB

    Array.init length (fun i ->
        let source = (getSource i)
        source.[i])

let mutate (organism: OrganismT) : OrganismT =
    let length = Array.length organism

    let min = 1
    let max = 1 + (((length |> float) * 0.05) |> int)

    let count = random.Next(min, max)

    let positions =
        Array.init count (fun _ -> random.Next(0, length))

    positions
    |> Array.iter (fun i ->
        organism.[i] <- random.Next(0, 255) |> byte)

    organism
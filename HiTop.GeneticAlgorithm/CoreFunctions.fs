[<AutoOpen>]
module HiTop.GeneticAlgorithm.CoreFunctions

open System.IO
open HiTop.VM

let private random = new System.Random()

// TODO: Async?

module Organism =
    let createRandom programLength : Organism =
        let buffer = Array.zeroCreate programLength
        random.NextBytes(buffer);
        buffer

    let private score (target: BinaryReader) engine =
        let rec score acc engine =
            let engine' = engine |> Engine.step

            if engine'.IsHalted then
                acc
            else
                match engine'.LastOutput with
                | None -> acc
                | Some(Byte x) ->
                    if x = target.ReadByte() then
                        score (acc + 1) engine'
                    else
                        score acc engine'
                | Some (Buffer buffer) ->
                    // Ensure that the array lengths are always the same by making sure that the
                    // remaining length in the target stream takes precedent over what the program
                    // is returning to us

                    let remainingLength =
                        let x = target.BaseStream.Length - target.BaseStream.Position
                        
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

                    score (acc + points) engine'

        score 0 engine

    let evaluate (settings: EvaluationSettings) (organism: Organism) : Fitness =
        let program = organism

        let engine' =
            settings.BaseEngine
            |> Engine.loadFromBuffer program

        let target = settings.Target

        let correct = engine' |> score target |> float
        let length = target.BaseStream.Length |> float

        correct / length

    let toEvaluated (settings: EvaluationSettings) (organism: Organism) : EvaluatedOrganism =
        (organism, organism |> evaluate settings)

    let crossover (parents: OrganismPair) : Organism =
        // Again like the chosen selection method, we really don't know how well this will work
        // until we actually try it.
        //
        // Here we have implemented a one-point crossover which result in a new program like:
        //
        //    [ ----- Parent A ------ | ----- Parent B ------ ]
        //
        // Where the mid-point is where the programs will crossover

        let parentA, parentB = parents
        
        // Assume they are equal length for now
        assert (parentA.Length = parentB.Length)

        let length = parentA.Length

        let midpoint = random.Next(0, length)

        let getSource i =
            if i < midpoint then
                parentA
            else
                parentB

        Array.init length (fun i ->
            let source = (getSource i)
            source.[i])

    let mutate (organism: Organism) : Organism =
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

module Population =

    let create (settings: PopulationSettings) : Population =
        Array.init settings.PopulationCount (fun _ -> Organism.createRandom settings.ProgramLength)

    let evaluate (settings: EvaluationSettings) (population: Population) : EvaluatedPopulation =
        population
        |> Array.map (Organism.toEvaluated settings)

    let select (population: EvaluatedPopulation) : SelectedPopulation =
        // TODO: We, of course, don't really know what selection method is better for this case,
        //       so we are just implementing Rank Selection just for now at least.

        let length = Array.length population

        // Sorts ascending
        population
        |> Array.sortInPlaceBy snd

        // The denominator of the rank fraction is twice the length
        // So that if for example you had a population of 4 organisms it would end up like this:
        //   4th -> 1/8
        //   3rd -> 2/8
        //   2nd -> 3/8
        //   1st -> 4/8
        let denom = length * 2 |> float
        
        // Replace the fitness with the probability percent
        let ranked =
            population
            |> Array.mapi (fun i (prgm, _) -> 
                let percent = (i |> float) / denom
                (prgm, percent))

        let getNextMate () =
            let x = random.NextDouble()
            
            let rec choose pos remaining =
                assert (pos < ranked.Length)

                let organism, rankPercent = ranked.[pos]
                let remaining' = remaining - rankPercent
                
                if remaining' <= 0.0 then
                    organism
                else
                    choose (pos + 1) remaining
            
            choose 0 x

        let mate organism =
            (organism, getNextMate ())

        population
        |> Array.map fst
        |> Array.map mate

    let private crossover (population: SelectedPopulation) : Population =
        population
        |> Array.map Organism.crossover

    let private mutate (population: Population) : Population =
        let rate = 0.05

        let willMutate () =
            random.NextDouble() <= rate

        population
        |> Array.map (fun x ->
            if willMutate () then
                x |> Organism.mutate
            else
                x)

    let reproduce (population: SelectedPopulation) : Population =
        population
        |> crossover
        |> mutate
module HiTop.GeneticAlgorithm.Population

open HiTop.VM

let create (settings: PopulationSettings) : PopulationT =
    let pregen = Helpers.prepareRandomByte settings.InstructionSet

    Array.init settings.PopulationCount
        (fun _ -> Organism.createRandom settings.ProgramLength pregen)

let evaluate (settings: EvaluationSettings) (population: PopulationT) : EvaluatedPopulation =
    population
    |> Array.map (Organism.toEvaluated settings)

let select (population: EvaluatedPopulation) : SelectedPopulation =
    // TODO: We, of course, don't really know what selection method is better for this case,
    //       so we are just implementing Rank Selection just for now at least.

    let length = Array.length population

    // Sorts ascending
    population
    |> Array.sortInPlaceBy snd

    // Replace the fitness with the probability percent
    let ranked =
        // The denominator of the rank fraction is length
        // Of course we will need to renormalize the ranks so that when summed they add up to 1.0
        let denom = length |> float

        let raw =
            population
            |> Array.mapi (fun i (prgm, _) -> 
                let numer = i + 1 |> float
                let value = numer / denom
                (prgm, value))

        let sum = raw |> Array.sumBy snd

        // Renormalize
        raw
        |> Array.map (fun (pgrm, value) -> (pgrm, value / sum))

        
    // Commented out for performance, though the ranked percentages should add up to 1.0
    //
    //   let sum = ranked |> Array.sumBy snd
    //   printfn "debug: ranked sum = %f" sum
    //   assert (sum - 1.0 < 0.00001)

    let getNextMate () =
        let x = random.NextDouble()
            
        let rec choose pos remaining =
            assert (pos < ranked.Length)

            let organism, rankPercent = ranked.[pos]
            let remaining' = remaining - rankPercent
                
            if remaining' <= 0.0 then
                organism
            else
                choose (pos + 1) remaining'
            
        choose 0 x

    let mate organism =
        (organism, getNextMate ())

    population
    |> Array.map fst
    |> Array.map mate

let private crossover (population: SelectedPopulation) : PopulationT =
    population
    |> Array.map Organism.crossover

let private mutate (population: PopulationT) : PopulationT =
    population
    |> Array.map Organism.mutate

let reproduce (population: SelectedPopulation) : PopulationT =
    population
    |> crossover
    |> mutate
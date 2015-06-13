module HiTop.GeneticAlgorithm.Population

open HiTop.VM

let create (settings: PopulationSettings) : PopulationT =
    let pregen = Helpers.prepareRandomByte settings.InstructionSet

    Array.init settings.PopulationCount
        (fun _ -> Organism.createRandom settings.ProgramLength pregen)

let evaluate (settings: EvaluationSettings) (population: PopulationT) : EvaluatedPopulation =
    population
    |> Array.map (Organism.toEvaluated settings)

let select = PopulationSelectionMethods.tournamentSelect

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
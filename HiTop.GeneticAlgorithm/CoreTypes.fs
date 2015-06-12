[<AutoOpen>]
module HiTop.GeneticAlgorithm.CoreTypes

type Organism = byte array

type Population = Organism array

type Fitness = float

type EvaluatedOrganism = Organism * Fitness

type EvaluatedPopulation = EvaluatedOrganism array

type OrganismPair = Organism * Organism

type SelectedPopulation = OrganismPair array


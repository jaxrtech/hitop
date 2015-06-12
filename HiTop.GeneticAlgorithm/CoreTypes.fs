[<AutoOpen>]
module HiTop.GeneticAlgorithm.CoreTypes

open System.IO
open HiTop.VM

type Organism = byte array

type PopulationSettings = {
     PopulationCount: int
     ProgramLength: int
}

type Population = Organism array

type Fitness = float

type EvaluationSettings = {
     Target: BinaryReader
     BaseEngine: Engine
}

type EvaluatedOrganism = Organism * Fitness

type EvaluatedPopulation = EvaluatedOrganism array

type OrganismPair = Organism * Organism

type SelectedPopulation = OrganismPair array


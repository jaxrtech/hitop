[<AutoOpen>]
module HiTop.GeneticAlgorithm.CoreTypes

open System.IO
open HiTop.VM

type OrganismT = byte array

type PopulationSettings = {
     PopulationCount: int
     ProgramLength: int
     InstructionSet: BuiltInstructionSet
}

type PopulationT = OrganismT array

type Fitness = float

type EvaluationSettings = {
     Target: BinaryReader
     TargetLength: int64
     InstructionSet: BuiltInstructionSet
}

type EvaluatedOrganism = OrganismT * Fitness

type EvaluatedPopulation = EvaluatedOrganism array

type OrganismPair = OrganismT * OrganismT

type SelectedPopulation = OrganismPair array


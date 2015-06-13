[<AutoOpen>]
module HiTop.GeneticAlgorithm.CoreFunctions

let internal random = new System.Random()

type TurnamentSelectionSettings = {
    Rounds: int
    EliteCount: int
}

module PopulationSelectionMethods =
    let rankSelect (population: EvaluatedPopulation) : SelectedPopulation =
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
    
    let tournamentSelect (settings: TurnamentSelectionSettings) (population: EvaluatedPopulation) : SelectedPopulation =
        let length = Array.length population

        let pickRandom () =
            let i = random.Next(0, length)
            population.[i]

        let decideTie a b =
            if random.NextDouble() < 0.5 then
                a
            else
                b

        let getNextWinner () =
            let rec f i best =
                match i with
                | 0 -> best
                | i ->
                    let a = best
                    let _, af = a

                    let b = pickRandom ()
                    let _, bf = b

                    let winner =
                        if af > bf then
                            a
                        elif af - bf < 1e-5 then
                            decideTie a b
                        else
                            b
                
                    f (i - 1) winner

            f settings.Rounds (pickRandom ())
            |> fst

        // Short descending by fitness
        population
        |> Array.sortInPlaceBy (snd >> (~-))

        population
        |> Array.mapi (fun i (x, _) ->
            if i < settings.EliteCount then
                (x, x)
            else
                (x, getNextWinner ()))
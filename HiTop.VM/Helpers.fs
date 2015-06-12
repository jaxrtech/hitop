module HiTop.VM.Helpers

let private random = new System.Random()

type RandomBytePregen = {
    InstructionSet: BuiltInstructionSet
    OperationBytes: byte array
}

let prepareRandomByte (instructionSet: BuiltInstructionSet) =
    // Calling this once to "pregen" the random byte generator will prevent this from being called
    // possibly millions of times which is totally unnecessary and would slow down startup times
    let operationBytes =
        instructionSet.FromByteCode
        |> Seq.choose (fun x ->
            let bytecode = x.Key
            let raw = x.Value
                     
            match bytecode with
            | Value _ -> None
            | _       -> Some(raw))

        |> Seq.toArray

    { InstructionSet = instructionSet
      OperationBytes = operationBytes }

let nextRandomByte (pregen: RandomBytePregen) =
    let operationBytes = pregen.OperationBytes

    // Try to even out the distribution of bytes and operations generated
    match random.NextDouble() with
    | x when x < 0.60 -> // 60% of the time: generate a raw byte
        let n = random.Next(0, System.Byte.MaxValue |> int)
        n |> byte
                
    | x -> // 40% of the time: generate an operation
        let i = random.Next(0, Array.length operationBytes)
        Seq.nth i operationBytes

let initWithRandomByte (pregen: RandomBytePregen) (i: int) =
    nextRandomByte pregen
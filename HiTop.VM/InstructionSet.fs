module HiTop.VM.InstructionSet

[<Literal>]
let MaxInstructionsCount = 256

type InstructionSetBuildFailure =
     | TooManyInstructions

let filledWithValues : UnbuiltInstructionSet =
    {0uy..255uy}
    |> Seq.fold (fun acc i -> acc |> Map.add i (Value(i))) Map.empty

let check instructions : Result<unit, InstructionSetBuildFailure> =
    if Seq.length instructions > MaxInstructionsCount
    then Failure TooManyInstructions
    else Success ()

let filledAtTop instructions =
    match check instructions with
    | Failure x -> Failure x
    | Success _ ->

    let padding = MaxInstructionsCount - Seq.length instructions
    assert (padding >= 0)

    let startPos = padding |> byte
    let endPos = 255uy
    let length = int(endPos - startPos) + 1

    assert (List.length instructions = length)
    
    let source = filledWithValues

    {startPos..endPos}
    |> Seq.fold
        (fun acc i ->
            let instructions, map = acc
            match instructions with
            | [] -> ([], map)
            | head::rest ->
                let map' = map |> Map.add i head
                (rest, map'))
    
        (instructions, source)
    
    |> snd
    |> Success

let build (instructions: UnbuiltInstructionSet) =
    let fromByteCode =
        instructions
        |> Seq.map (fun pair -> (pair.Value, pair.Key))
        |> Map.ofSeq

    { FromByte = instructions
      FromByteCode = fromByteCode }
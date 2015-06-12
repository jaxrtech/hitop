module HiTop.VM.InstructionSet

[<Literal>]
let MaxInstructionsCount = 256

let empty =
    { FromByte = Map.empty
      FromByteCode = Map.empty }

let filledWithValues : UnbuiltInstructionSet =
    {0uy..255uy}
    |> Seq.fold (fun acc i -> acc |> Map.add i (Value(i))) Map.empty

let filledAtTop instructions =
    assert (List.length instructions <= MaxInstructionsCount)

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

let build (instructions: UnbuiltInstructionSet) =
    let fromByteCode =
        instructions
        |> Seq.map (fun pair -> (pair.Value, pair.Key))
        |> Map.ofSeq

    { FromByte = instructions
      FromByteCode = fromByteCode }
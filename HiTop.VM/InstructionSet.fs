module HiTop.VM.InstructionSet

open System.Collections.Generic
open System.Collections.ObjectModel
open HiTop.VM.CoreTypes

let empty =
    let mapping = new Dictionary<byte, ByteCode>()
    new ReadOnlyDictionary<byte, ByteCode>(mapping)

let build (instructions: UnbuiltInstructionSet) : BuiltInstructionSet =
    assert (List.length instructions <= 256)

    let rec f (acc: Dictionary<byte, ByteCode>) rest i =
        match rest with
        | [] -> acc
        | head::rest ->
            acc.[i] <- head
            assert (i <> 255uy) // no wrap over
            f acc rest (i + 1uy)

    // Add padding at the beginning for unencoded byte literals if we have room for it
    let padding = 256 - instructions.Length

    assert (padding > 0)

    let mapping = f (new Dictionary<byte, ByteCode>()) (instructions) (byte (padding - 1))

    upcast new ReadOnlyDictionary<byte, ByteCode>(mapping)

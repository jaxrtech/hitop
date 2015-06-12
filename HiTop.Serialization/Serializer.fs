module HiTop.Serialization.Serializer

open System.IO

let serializeTo (output: Stream) (context: SerializationContext) =
    assert (output.CanWrite)

    let writer = new BinaryWriter(output)
    context |> ContextSerializer.write writer

let derserializeFrom (input: Stream) =
    assert (input.CanRead)

    let reader = new BinaryReader(input)
    ContextSerializer.read reader

    
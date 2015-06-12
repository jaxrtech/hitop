module HiTop.Serialization

open System.IO

let serializeTo (program: byte []) (stream: Stream) =
    stream.Write(program, 0, program.Length)

let derserializeFrom (stream: Stream) =
    let result = Array.zeroCreate (stream.Length |> int)
    stream.Read(result, 0, result.Length)
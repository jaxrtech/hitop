[<AutoOpen>]
module HiTop.Serialization.CoreTypes

open System.IO
open System.Text

type Result<'TSuccess,'TFailure> = 
     | Success of 'TSuccess
     | Failure of 'TFailure

let MagicSignature =
    "HITOP"
    |> Encoding.ASCII.GetBytes

type SerializationContext = {
     FileName: string
     FileSize: int
     Program: byte []
}

type DeserializationError =
     | BadMagicSignature
     | BadFileSize
     | BadFileName
     | UnpectedEndOfFile

type SerializationNode = {
     write: SerializationContext -> BinaryWriter -> unit
     read: SerializationContext -> BinaryReader -> Result<SerializationContext, DeserializationError>
}

[<AutoOpen>]
module HiTop.Serialization.CoreFunctions

module Context =
    let empty =
        { FileName = ""
          FileSize = 0
          Program = Array.empty }
[<AutoOpen>]
module HiTop.VM.CoreFuntions

module StackElement =
    let toString x = sprintf "%d" x

    let boolAsValue = function
    | false -> 0uy
    | true  -> 1uy

module Stack =
    let toString (stack: Stack) =
        stack
        |> Seq.map StackElement.toString
        |> String.concat ", "
        |> fun x -> "[" + x + "]"

module Output =
    let appendTo (buffer: byte array) (output: Output option) =
        match output with
        | Some(Byte(x)) -> [| x |] |> Array.append buffer
        | Some(Buffer(x)) -> x |> Array.append buffer
        | None -> buffer
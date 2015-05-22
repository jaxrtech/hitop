[<AutoOpen>]
module HiTop.VM.CoreFuntions

module LambdaArg =
    let isPlaceholder = function
    | Placeholder -> true
    | _ -> false

module StackElement =
    let equalAsValue (a: StackElement) (b: StackElement) =
        match (a, b) with
        | Value(x), Value(y) -> x = y
        | _ -> false

    let equalAsOptValue (a: StackElement) (b: StackElement option) =
        match (a, b) with
        | Value(x), Some(Value(y)) -> x = y
        | _ -> false

    let rec toString = function
    | Value x -> sprintf "%d" x
    | Instruction(x) -> sprintf "[%s]" x.ShortName
    | Lambda(x) ->
        let named = sprintf "λ[%s]"

        match x.Args with
        | args when args |> List.forall LambdaArg.isPlaceholder ->
            named x.ShortName
        
        | args ->
            args
            |> List.map (function
                   | Placeholder -> "?"
                   | StackElement x -> toString x)
            |> String.concat " "
            |> sprintf "λ[%s]"

        | [] ->
            named x.ShortName

module Stack =
    let toString (stack: Stack) =
        stack
        |> Seq.map StackElement.toString
        |> String.concat ", "
        |> fun x -> "[" + x + "]"
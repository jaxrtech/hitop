[<AutoOpen>]
module HiTop.VM.CoreFuntions

module LambdaArg =
    let isPlaceholder = function
    | Placeholder -> true
    | _ -> false

    let padding (n: int) =
        Array.create n Placeholder

    let padTo (n: int) (args: LambdaArg array) =
        assert (args.Length <= n)
        
        Array.append
            args
            (Array.create (n - args.Length) Placeholder)

module StackElement =
    let isValue = function
    | Value _ -> true
    | _ -> false

    let equalAsValue (a: StackElement) (b: StackElement) =
        match (a, b) with
        | Value(x), Value(y) -> x = y
        | _ -> false

    let equalAsOptValue (a: StackElement) (b: StackElement option) =
        match (a, b) with
        | Value(x), Some(Value(y)) -> x = y
        | _ -> false

    let boolAsValue = function
    | false -> Value(0uy)
    | true -> Value(1uy)

    let rec toString = function
    | Value x -> sprintf "%d" x
    | Instruction(x) -> sprintf "[%s]" x.ShortName
    | Lambda(x) ->
        let named = sprintf "λ[%s]"

        match x.Args with
        | args when args |> Array.forall LambdaArg.isPlaceholder ->
            named x.ShortName
        
        | args ->
            args
            |> Array.map (function
                   | Placeholder -> "?"
                   | StackElement x -> toString x)
            |> String.concat " "
            |> sprintf "λ[%s]"

        | [||] ->
            named x.ShortName

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
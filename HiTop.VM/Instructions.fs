module HiTop.VM.Instructions

open HiTop.VM.CoreTypes
open HiTop.VM.Stack

let private make name op =
    { ShortName = name; Op = op }

let buildLambdaFromPartial name engine n f lambdaArgs resultArgs =
    let finalLambda engine resultArgs =
        assert ((resultArgs |> Array.length) = n)

        // Drop argument `y` and the lambda itself from the stack
        engine.Stack |> StrictStack.dropn 2
        f engine resultArgs

    let rec rootLambda engine lambdaArgs resultArgs =
                 
        let buildState lambdaArgs lambda =
            { ShortName = name
              Args = lambdaArgs |> Array.map StackElement |> LambdaArg.padTo n
              Lambda = lambda }
                          
        let pushLambda stack lambdaArgs lambda =
            let state = buildState lambdaArgs lambda
            engine.Stack |> StrictStack.push (Lambda(state))

        (fun engine ->
            // Drop argument `x` and the lambda itself from the stack
            engine.Stack |> StrictStack.dropn 2

            let lambda engine =
                let x = engine.Stack |> StrictStack.peekAt 0
                match x with
                | Some(Value x as a) ->
                    let engine' =
                        let resultArgs' = Array.append resultArgs [|x|]
                        let lambdaArgs' = Array.append lambdaArgs [|a|]

                        if resultArgs'.Length = n then
                            finalLambda engine resultArgs'
                        else
                            // Drop argument `x` and the lambda itself from the stack
                            engine.Stack |> StrictStack.dropn 2

                            let lambda' = rootLambda engine lambdaArgs' resultArgs'

                            pushLambda engine.Stack lambdaArgs' lambda'
                            engine

                    Some(engine')

                | _ -> None

            pushLambda engine.Stack lambdaArgs lambda
            Some(engine))

    rootLambda engine lambdaArgs resultArgs

let buildLambdaFromEmpty name engine n f =
    buildLambdaFromPartial name engine n f (Array.empty) (Array.empty)

let buildLambdaState name engine size f = 
    { ShortName = name
      Args = LambdaArg.padding size
      Lambda = buildLambdaFromEmpty name engine size f }

let private ``1->1 (raw)`` name onNormal onLambda = make name (fun engine ->
    let x0 = engine.Stack |> StrictStack.peek
        
    match x0 with
    | Some(Value x) ->
        onNormal engine x

    | _ ->
        let lambda engine =
            // HACK: Right now we are just assuming the lambda is the 1th item and the arg is the
            //       0th item on the stack so it looks like:
            //        ... [lambda@1] [arg@0]

            let x = engine.Stack |> StrictStack.peek
            match x with
            | Some(Value x) ->
                Some(onLambda engine x)

            | _ -> None

        let state = 
            { ShortName = name
              Args = [| Placeholder |]
              Lambda = lambda }

        engine.Stack |> StrictStack.push (Lambda(state))
        engine)

let private ``1->1`` name f =
    let resultOf engine x =
        engine.Stack |> StrictStack.push (f x)
        engine

    ``1->1 (raw)`` name
        (fun engine x ->
            x |> resultOf engine)

        (fun engine x ->
            engine.Stack |> StrictStack.dropn 2
            x |> resultOf engine)

// TODO: Generalize on the number of arguments

type ArgumentSpec<'a> = {
    Count: int
    selector: StackElement -> bool
    unwrapper: StackElement -> 'a option
}

let private readStackArgs stack argumentSpec =
    let elements = 
        stack
        |> StrictStack.peekn argumentSpec.Count

           // Only keep taking arguments while they are of the selected element type
        |> Seq.takeWhile argumentSpec.selector
        |> Seq.toList

    let args =
        elements
           // Unwrap to list of bytes
        |> List.choose argumentSpec.unwrapper
        |> List.toArray

    assert (List.length elements = Array.length args)

    let n = List.length elements
    (n, elements, args)

let private ``n->1`` name argumentSpec f = make name (fun engine ->
    let x = engine.Stack |> StrictStack.peekn argumentSpec.Count

    let g engine args =
        assert (Array.length args = argumentSpec.Count)
        engine.Stack |> StrictStack.push (f args)
        engine

    let argsCount, lambdaArgs, resultArgs = readStackArgs engine.Stack argumentSpec

    match lambdaArgs with
    | args when argsCount = argumentSpec.Count ->
        engine.Stack |> StrictStack.dropn argumentSpec.Count
        g engine resultArgs

    | args when argsCount < argumentSpec.Count ->
        engine.Stack |> StrictStack.dropn argsCount
        let start = buil

    | args -> // when List.length args = 0 
        let state = buildLambdaState name engine argumentSpec.Count g
        engine.Stack |> StrictStack.push (Lambda(state))
        engine)


let private ``1->1:bool`` name f = ``1->1`` name (fun x -> f x |> StackElement.boolAsValue)

let private ``2->1:bool`` name f = ``2->1`` name (fun x0 x1 -> f x0 x1 |> StackElement.boolAsValue)

let private ``2->1:byte`` name f = ``2->1`` name (fun x0 x1 -> Value(f x0 x1))

//

module Arithmetic =
    let add = ``2->1:byte`` "add" (+)

    let sub = ``2->1:byte`` "sub" (-)

    let mul = ``2->1:byte`` "mul" (*)

    let div = ``2->1:byte`` "div" (fun a -> function
                            | 0uy -> 0uy
                            | b   -> a / b)

    let ``mod`` = ``2->1:byte`` "mod" (%)

    let all =
        add :: sub :: mul :: div :: ``mod`` :: []

module Stack =
    let dup = make "dup" (fun engine ->
        let x0 = engine.Stack |> StrictStack.peek
        
        match x0 with
        | Some(x) ->
            engine.Stack |> StrictStack.push x
            engine

        | _ -> engine)

    let all = dup :: []

module Output =
    let private withOutputOf x engine = { engine with LastOutput = Some(Byte(x)) }

    /// Instruction that reads a value `x` and then outputs it without dropping `x` from the stack
    let out = ``1->1 (raw)`` "out"
                (fun engine x -> 
                    // Leave input on the stack during normal execution
                    engine |> withOutputOf x)

                (fun engine x ->
                    // Drop only the lambda itself from the stack and not arg `x`
                    engine.Stack |> StrictStack.dropAt 1
                    engine |> withOutputOf x)

    let put = ``1->1 (raw)`` "put"
                (fun engine x ->
                    // Drop the input during normal execution
                    engine.Stack |> StrictStack.drop
                    engine |> withOutputOf x)

                (fun engine x ->
                    // Drop argument `x` and the lambda itself from the stack
                    engine.Stack |> StrictStack.dropn 2
                    engine |> withOutputOf x)

    let all = out :: put :: []

module Comparison =
    /// Equals zero
    let cez = ``1->1:bool`` "cez" ((=) 0uy)

    /// Not equal to zero
    let cnz = ``1->1:bool`` "cnz" ((<>) 0uy)

    /// Equal to
    let ceq = ``2->1:bool`` "ceq" (=)
    
    /// Not equal to
    let cnq = ``2->1:bool`` "cnq" (<>)
        
    /// Less than
    let clt = ``2->1:bool`` "clt" (<)
            
    /// Less than or equal to
    let cle = ``2->1:bool`` "cle" (<=)
                
    /// Greater than
    let cgt = ``2->1:bool`` "cgt" (>)

    /// Greater than
    let cge = ``2->1:bool`` "cge" (>=)

    let all = cez :: cnz :: ceq :: clt :: cle :: cgt :: cge :: []

module Jumps =
    // Jump to start (reset)
    let rst = make "rst" (fun engine ->
        { engine with NextReadAddress = 0L })

    // Jump to start (reset)
    let jmp_64 =
        let name = "jmp_64"
        make name (fun engine ->
            let size = 8 // in bytes

            let args =
                engine.Stack
                |> StrictStack.peekn size
               
                   // Only keep taking arguments while they are of `Value`
                |> Seq.takeWhile (function
                                    | Value(x) -> true
                                    | _ -> false)
                |> Seq.toList

                   // Unwrap to list of bytes
                |> List.choose (function
                                    | Value(x) -> Some(x)
                                    | _ -> None)
        
            let f engine x =
                let address =
                    let i =
                        // Since the value of the address is always a signed int64 (don't ask me why),
                        // we want to have the offset position only go to the maximum of a int64 even
                        // though we always want it >= 0 when reading the values from the VM.
                    
                        let x = System.BitConverter.ToUInt64(args |> List.toArray, 0)
                        x % (uint64 System.Int64.MaxValue) |> int64

                    // Prevent divide by zero
                    match engine.Program.BaseStream.Length with
                    | 0L -> 0L
                    | n -> (uint64 i) % (uint64 n) |> int64

                { engine with NextReadAddress = address }

            match args with
            | args when (args |> List.length) = size ->
                f engine args

            | _ ->
                let state = buildLambdaState name engine size f
                engine.Stack |> StrictStack.push (Lambda(state))
                engine)


let all = Arithmetic.all @ Stack.all @ Output.all @ Comparison.all
module HiTop.VM.Instructions

open HiTop.VM.CoreTypes
open HiTop.VM.Stack

let private make name op =
    { ShortName = name; Op = op }

let private arith2 name f = make name (fun engine ->
    let x0 = engine.Stack |> StrictStack.peekAt 0
    let x1 = engine.Stack |> StrictStack.peekAt 1

    match (x1, x0) with
    | Some(Value x1), Some(Value x0) ->
        engine.Stack |> StrictStack.dropn 2
        engine.Stack |> StrictStack.push (Value(f x1 x0))
        engine

    | _, _ ->
        let lambda engine =
            // HACK: Right now we are just assuming the lambda is the 1th item and the arg is the
            //       0th item on the stack so it looks like:
            //        ... [lambda@1] [arg@0]

            let x = engine.Stack |> StrictStack.peekAt 0
            match x with
            | Some(Value x as a) ->
                // Drop argument `x` and the lambda itself from the stack
                engine.Stack |> StrictStack.dropn 2

                let innerlambda engine =
                    let y = engine.Stack |> StrictStack.peekAt 0
                    match y with
                    | Some(Value y) ->
                        // Drop argument `y` and the lambda itself from the stack
                        engine.Stack |> StrictStack.dropn 2
                        engine.Stack |> StrictStack.push (Value(f y x))
                        Some(engine)

                    | _ -> None

                let state = 
                    { ShortName = name
                      Args = [StackElement(a); Placeholder]
                      Lambda = innerlambda }

                engine.Stack |> StrictStack.push (Lambda(state))
                Some(engine)

            | _ -> None

        let state = 
            { ShortName = name
              Args = [Placeholder; Placeholder]
              Lambda = lambda }

        engine.Stack |> StrictStack.push (Lambda(state))
        engine
)

module Arithmetic =
    let add = arith2 "add" (+)

    let sub = arith2 "sub" (-)

    let mul = arith2 "mul" (*)

    let div = arith2 "div" (fun a -> function
                            | 0uy -> 0uy
                            | b   -> a / b)

    let ``mod`` = arith2 "mod" (%)

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
    let private output name onNormalResult onLambdaResult = make name (fun engine ->
        let x0 = engine.Stack |> StrictStack.peek
        
        let f x =
            { engine with LastOutput = Some(Byte(x)) }

        match x0 with
        | Some(Value x) ->
            onNormalResult engine
            f x

        | _ ->
            let lambda engine =
                // HACK: Right now we are just assuming the lambda is the 1th item and the arg is the
                //       0th item on the stack so it looks like:
                //        ... [lambda@1] [arg@0]

                let x = engine.Stack |> StrictStack.peek
                match x with
                | Some(Value x) ->
                    onLambdaResult engine
                    Some(f x)

                | _ -> None

            let state = 
                { ShortName = name
                  Args = [Placeholder]
                  Lambda = lambda }

            engine.Stack |> StrictStack.push (Lambda(state))
            engine)

    /// Instruction that reads a value `x` and then outputs it without dropping `x` from the stack
    let out = output "out"
                (fun _ -> 
                    // Leave input on the stack during normal execution
                    ())

                (fun engine ->
                    // Drop only the lambda itself from the stack and not arg `x`
                    engine.Stack |> StrictStack.dropAt 1)

    let put = output "put"
                (fun engine ->
                    // Drop the input during normal execution
                    engine.Stack |> StrictStack.drop)

                (fun engine ->
                    // Drop argument `x` and the lambda itself from the stack
                    engine.Stack |> StrictStack.dropn 2)

    let all = out :: put :: []

let all = Arithmetic.all @ Stack.all @ Output.all
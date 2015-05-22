module HiTop.VM.Instructions

open HiTop.VM.CoreTypes
open HiTop.VM.Stack

let private make name op =
    { ShortName = name; Op = op }

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
              Args = [Placeholder]
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

let private ``2->1`` name f = make name (fun engine ->
    let x0 = engine.Stack |> StrictStack.peekAt 0
    let x1 = engine.Stack |> StrictStack.peekAt 1

    match (x1, x0) with
    | Some(Value x1), Some(Value x0) ->
        engine.Stack |> StrictStack.dropn 2
        engine.Stack |> StrictStack.push (f x1 x0)
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
                        engine.Stack |> StrictStack.push (f y x)
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

let all = Arithmetic.all @ Stack.all @ Output.all @ Comparison.all
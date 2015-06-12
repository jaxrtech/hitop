module HiTop.VM.Instructions

open HiTop.VM.CoreTypes
open HiTop.VM.Stack

let inline private make name op =
    { ShortName = name; Op = op } |> Instruction

let inline private peek1 name f = make name (fun engine ->
    let stack = engine.Stack

    let x = stack |> StrictStack.peek

    match x with
    | Some(x) ->
        Some(f engine x)

    | _ -> None)

let inline private pop1 name f = make name (fun engine ->
    let stack = engine.Stack

    let x = stack |> StrictStack.peek

    match x with
    | Some(x) ->
        stack |> StrictStack.drop
        Some(f engine x)

    | _ -> None)

let inline private pop2 name f = make name (fun engine ->
    let stack = engine.Stack

    let x1 = stack |> StrictStack.peekAt 1
    let x0 = stack |> StrictStack.peekAt 0

    match (x1, x0) with
    | Some(x1), Some(x0) ->
        stack |> StrictStack.dropn 2
        Some(f engine x1 x0)

    | _ -> None)

let inline private ``1->1`` name f = pop1 name (fun engine x0 ->
    let stack = engine.Stack

    stack |> StrictStack.push (f x0)
    engine)

let inline private ``2->1`` name f = pop2 name (fun engine x0 x1 ->
    let stack = engine.Stack

    stack |> StrictStack.push (f x0 x1)
    engine)

let inline private ``1->1:bool`` name f = ``1->1`` name (fun x0 ->
    f x0 |> StackElement.boolAsValue)

let inline private ``2->1:bool`` name f = ``2->1`` name (fun x0 x1 ->
    f x0 x1 |> StackElement.boolAsValue)

//

module Arithmetic =
    let inc = ``1->1`` "inc" (fun x -> x - 1uy)

    let dec = ``1->1`` "dec" (fun x -> x + 1uy)

    let add = ``2->1`` "add" (+)

    let sub = ``2->1`` "sub" (-)

    let mul = ``2->1`` "mul" (*)

    let div = ``2->1`` "div" (fun a -> function
                              | 0uy -> 0uy
                              | b   -> a / b)

    let ``mod`` = ``2->1`` "mod" (fun a -> function
                                  | 0uy -> 0uy
                                  | b   -> a % b)

    let all =
        inc :: dec :: add :: sub :: mul :: div :: ``mod`` :: []

module Stack =
    let dup = make "dup" (fun engine ->
        let x0 = engine.Stack |> StrictStack.peek
        
        match x0 with
        | Some(x) ->
            engine.Stack |> StrictStack.push x
            Some(engine)

        | _ -> None)

    let all = dup :: []

module Output =
    let private withOutputOf x engine = { engine with LastOutput = Some(Byte(x)) }

    /// Instruction that reads a value `x` and then outputs it without dropping `x` from the stack
    let out = peek1 "out" (fun engine x -> engine |> withOutputOf x)
    
    /// Instruction that reads a value `x` and then outputs it dropping `x` from the stack
    let put = pop1 "put" (fun engine x -> engine |> withOutputOf x)

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
        engine |> Engine.setNextReadAddress 0L |> Some)

    let all = rst :: LoopBeginMarker :: LoopEndMarker :: []

module Markers =
    let all = EncodedByteMarker :: []

let all =
       Markers.all
     @ Jumps.all
     @ Arithmetic.all
     @ Stack.all
     @ Output.all
     @ Comparison.all

let allExceptJumps =
       Markers.all
     @ Arithmetic.all
     @ Stack.all
     @ Output.all
     @ Comparison.all

// At startup, run check to ensure all the short names are unique
let private uniqueCount =
    all
    |> List.map ByteCode.toString
    |> Seq.distinct
    |> Seq.length

assert (all.Length = uniqueCount)
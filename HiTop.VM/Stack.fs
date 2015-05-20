module HiTop.VM.Stack

open System.Collections.Generic
open HiTop.VM.CoreTypes

type Stack = IList<StackElement>

let create (): IList<StackElement> =
    upcast new ResizeArray<StackElement>()

let count (stack: Stack) =
    stack.Count

let isEmpty (stack: Stack) =
    stack.Count = 0

let notEmpty (stack: Stack) =
    stack |> isEmpty |> not

let private indexFromPos (pos: int) (stack: Stack) =
    assert (stack.Count >= 0)

    match stack.Count with
    | 0 -> 0
    | _ -> (stack.Count - 1) - pos

let peekAt (i: int) (stack: Stack) =
    if notEmpty stack then
        let pos = i % stack.Count
        let i' = stack |> indexFromPos pos
        Some (stack.[i'])
    else
        None

let peek (stack: Stack) =
    stack |> peekAt 0

let dropAt (i: int) (stack: Stack) =
    let pos = i % stack.Count
    let i' = stack |> indexFromPos pos
    stack.RemoveAt(i')
    
let dropn (n: int) (stack: Stack) =
    let n' = match stack.Count with
             | count when n <= 0     -> 0
             | count when n >= count -> count
             | _ -> n

    let rec f = function
    | 0 -> ()
    | n ->
        stack |> dropAt 0
        f (n - 1)
    
    match n' with
    | 0 -> ()
    | _ -> f n

let drop (stack: Stack) =
    stack |> dropn 1

let popAt (i: int) (stack: Stack) =
    stack 
    |> peekAt i
    |> Option.map (fun x ->
        stack |> dropAt i
        x)

let pop (stack: Stack) =
    stack |> popAt 0

let pushAt (i: int) (element: StackElement) (stack: Stack) =
    assert (stack.Count >= 0)

    // This prevents `i % 0` which would result in a DivideByZeroException
    let pos = match stack.Count with
              | 0 -> 0
              | n -> i % n

    let i' =
        // Make sure the displaced element ends up on the right not the lefts
        let x = stack |> indexFromPos pos
        match stack.Count with
        | 0 -> 0
        | _ -> x + 1

    stack.Insert(i', element)

let push (element: StackElement) (stack: Stack) =
    stack |> pushAt 0 element
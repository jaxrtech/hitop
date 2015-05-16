module HiTop.VM.Stack

open System.Collections.Generic
open HiTop.VM.CoreTypes

type Stack = IList<StackElement>

let create () =
    new List<StackElement>()

let count (stack: Stack) =
    stack.Count

let isEmpty (stack: Stack) =
    stack.Count = 0

let notEmpty (stack: Stack) =
    stack |> isEmpty |> not

let peekAt (i: int) (stack: Stack) =
    if notEmpty stack then
        let i = i % stack.Count
        Some (stack.[i])
    else
        None

let peek (stack: Stack) =
    stack |> peekAt 0
    
let dropn (n: int) (stack: Stack) =
    let n' = match stack.Count with
             | count when n <= 0     -> 0
             | count when n >= count -> count
             | _ -> n

    let rec f = function
    | 0 -> ()
    | n ->
        stack.RemoveAt(0)
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
        stack.RemoveAt(i)
        x)

let pop (stack: Stack) =
    stack |> popAt 0

let pushAt (i: int) (element: StackElement) (stack: Stack) =
    assert (stack.Count >= 0)

    // This prevents `i % 0` which would result in a DivideByZeroException
    let i' = match stack.Count with
             | 0 -> 0
             | n -> i % n

    stack.Insert(i', element)

let push (element: StackElement) (stack: Stack) =
    stack |> pushAt 0 element
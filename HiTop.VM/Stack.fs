module HiTop.VM.Stack

open System.Collections.Generic
open HiTop.VM.CoreTypes

type Stack = List<StackElement>

let create () =
    new List<StackElement>()

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
    let rec f = function
    | 0 -> ()
    | _ ->
        stack.RemoveAt(0)
        f (n - 1)
    
    if n <= stack.Count then f n

let drop (stack: Stack) =
    stack |> dropn 1

let popAt (i: int) (stack: Stack) =
    match stack |> peekAt i with
    | None -> None
    | Some x ->
        stack.RemoveAt(i)
        Some x

let pop (stack: Stack) =
    stack |> popAt 0

let pushAt (i: int) (element: StackElement) (stack: Stack) =
    let i = i % stack.Count

    match i with
    | 0 when isEmpty stack  -> ()
    | 0 when notEmpty stack -> stack.Insert(0, element)
    | i                     -> stack.Insert(i, element)

let push (element: StackElement) (stack: Stack) =
    stack |> pushAt 0 element
module HiTop.VM.Stack

open HiTop.VM.CoreTypes

type StackOperationBaseSet = {
    peekAt: int -> Stack -> StackElement option
    dropAt: int -> Stack -> unit
    pushAt: int -> StackElement -> Stack -> unit
}

let create (): Stack =
    new ResizeArray<StackElement>()

let empty = create ()

let count (stack: Stack) =
    stack.Count

let isEmpty (stack: Stack) =
    stack.Count = 0

let notEmpty (stack: Stack) =
    stack |> isEmpty |> not

module private DerivedOperations =
    let peek (ops: StackOperationBaseSet) (stack: Stack) =
        stack |> ops.peekAt 0


    let dropn (ops: StackOperationBaseSet) (n: int) (stack: Stack) =
        let n' = match stack.Count with
                 | count when n <= 0     -> 0
                 | count when n >= count -> count
                 | _ -> n

        let rec f = function
        | 0 -> ()
        | n ->
            stack |> ops.dropAt 0
            f (n - 1)
    
        match n' with
        | 0 -> ()
        | _ -> f n

    let drop (ops: StackOperationBaseSet) (stack: Stack) =
        stack |> dropn ops 1

    let popAt (ops: StackOperationBaseSet) (i: int) (stack: Stack) =
        stack 
        |> ops.peekAt i
        |> Option.map (fun x ->
            stack |> ops.dropAt i
            x)

    let pop (ops: StackOperationBaseSet) (stack: Stack) =
        stack |> popAt ops 0

    let push (ops: StackOperationBaseSet) (element: StackElement) (stack: Stack) =
        stack |> ops.pushAt 0 element

    /// Peeks at the top `n` elements on the stack returning a list in ascending index order so that
    /// is would look like `[peekAt 0, peekAt 1, ..., peekAt n-1]`.
    ///
    /// If the stack has less then `n` elements, then only the maximum number of element is returned
    let peekn (ops: StackOperationBaseSet) (n: int) (stack: Stack) =
        {0..n-1}
        |> Seq.map (fun i -> stack |> ops.peekAt i)
        |> Seq.choose id // filter out `None` by choosing only `Some(x)`

    let peekAtHook (ops: StackOperationBaseSet) (i: int) (stack: Stack) =
        // NOTE: There is no way to check that the `peekAt` and `popAt` will return the save value since
        //       since `StackElement` does not have structural equality

        (stack |> ops.peekAt i,
         fun () -> stack |> popAt ops i |> ignore)

    let peekHook (ops: StackOperationBaseSet) (stack: Stack) =
        stack |> peekAtHook ops 0

type StackOperations = {
    peek: Stack -> StackElement option
    peekn: int -> Stack -> seq<StackElement>
    peekAt: int -> Stack -> StackElement option
    peekAtHook: int -> Stack -> StackElement option * (unit -> unit)
    peekHook: Stack -> StackElement option * (unit -> unit)
    
    drop: Stack -> unit
    dropn: int -> Stack -> unit
    dropAt: int -> Stack -> unit
    
    push: StackElement -> Stack -> unit
    pushAt: int -> StackElement -> Stack -> unit
    
    pop: Stack -> StackElement option
    popAt: int -> Stack -> StackElement option

}

let buildOperations (ops: StackOperationBaseSet) =
    { peekAt = ops.peekAt
      dropAt = ops.dropAt
      pushAt = ops.pushAt
      
      peek = DerivedOperations.peek ops
      dropn = DerivedOperations.dropn ops
      drop = DerivedOperations.drop ops
      popAt = DerivedOperations.popAt ops
      pop = DerivedOperations.pop ops
      push = DerivedOperations.push ops
      peekn = DerivedOperations.peekn ops
      peekAtHook = DerivedOperations.peekAtHook ops
      peekHook = DerivedOperations.peekHook ops }

//

let private indexFromPos (pos: int) (stack: Stack) =
    assert (stack.Count >= 0)

    match stack.Count with
    | 0 -> 0
    | _ -> (stack.Count - 1) - pos

let WrappedStack =
    let peekAt (i: int) (stack: Stack) =
        if notEmpty stack then
            let pos = i % stack.Count
            let i' = stack |> indexFromPos pos
            Some (stack.[i'])
        else
            None

    let dropAt (i: int) (stack: Stack) =
        if notEmpty stack then
            let pos = i % stack.Count
            let i' = stack |> indexFromPos pos
            stack.RemoveAt(i')
        else
            ()

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

    let operations = {
        StackOperationBaseSet.peekAt = peekAt
        dropAt = dropAt
        pushAt = pushAt
    }

    buildOperations operations

let StrictStack =
    let isIndexInBounds (stack: Stack) (i: int) =
        i = 0 || (i > 0 && i < stack.Count)

    let peekAt (i: int) (stack: Stack) =
        if notEmpty stack && i |> isIndexInBounds stack then
            let i' = stack |> indexFromPos i
            Some (stack.[i'])
        else
            None

    let dropAt (i: int) (stack: Stack) =
        if notEmpty stack && i |> isIndexInBounds stack then
            let i' = stack |> indexFromPos i
            stack.RemoveAt(i')
        else
            ()

    let pushAt (i: int) (element: StackElement) (stack: Stack) =
        assert (stack.Count >= 0)
        
        if i |> isIndexInBounds stack then
            let i' =
                // Make sure the displaced element ends up on the right not the lefts
                let x = stack |> indexFromPos i
                match stack.Count with
                | 0 -> 0
                | _ -> x + 1

            stack.Insert(i', element)
        else
            ()

    let operations = {
        StackOperationBaseSet.peekAt = peekAt
        dropAt = dropAt
        pushAt = pushAt
    }

    buildOperations operations
    
    
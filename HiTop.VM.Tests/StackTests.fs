module HiTop.VM.Tests.``A stack``

open Xunit
open FsUnit.Xunit
open HiTop.VM
open HiTop.VM.CoreTypes

let shouldEqualAsValue x y =
    equalAsOptValue (Value(x)) y
    |> should be True

[<Fact>]
let ``when created should be empty.`` () =
    let s = Stack.create ()
    s |> Stack.isEmpty |> should be True
    s |> Stack.notEmpty |> should be False
    s |> Stack.count |> should equal 0

[<Fact>]
let ``when pushed and popped should be empty.`` () =
    let s = Stack.create ()
    
    s |> Stack.push (CoreTypes.Value(10uy))
    s |> Stack.drop
    
    s |> Stack.count |> should equal 0

[<Fact>]
let ``when pushed and poped should be in the correct order.`` () =
    let s = Stack.create ()

    s |> Stack.push (Value(3uy))
    s |> Stack.push (Value(2uy))
    s |> Stack.push (Value(1uy))

    let popShouldEqual x =
        s
        |> Stack.pop
        |> equalAsOptValue (Value(x))
        |> should be True

    [1uy; 2uy; 3uy]
    |> List.iter popShouldEqual

    s |> Stack.isEmpty |> should be True

[<Fact>]
let ``when peeked should return the correct value without removing any values from the stack.`` () =
    let s = Stack.create ()

    s |> Stack.push (Value(3uy))
    s |> Stack.push (Value(2uy))
    s |> Stack.push (Value(1uy))

    s
    |> Stack.peekAt 2
    |> shouldEqualAsValue 3uy

    s
    |> Stack.peekAt 1
    |> shouldEqualAsValue 2uy

    s
    |> Stack.peekAt 0
    |> shouldEqualAsValue 1uy

    s
    |> Stack.peek
    |> shouldEqualAsValue 1uy

    s |> Stack.notEmpty |> should be True

[<Fact>]
let ``when pushed at 0 index and poped at an index, the values should be the same.`` () =
    let s = Stack.create ()

    s |> Stack.pushAt 0 (Value(3uy))
    s |> Stack.pushAt 0 (Value(2uy))
    s |> Stack.pushAt 0 (Value(1uy))

    s
    |> Stack.popAt 2
    |> shouldEqualAsValue 3uy

    s
    |> Stack.popAt 1
    |> shouldEqualAsValue 2uy

    s
    |> Stack.popAt 0
    |> shouldEqualAsValue 1uy

    s |> Stack.isEmpty |> should be True


[<Fact>]
let ``when popping or peeking at an empty stack should result in None and not run out of bounds.`` () =
    let s = Stack.create ()

    let check f =
        s
        |> f
        |> Option.isNone
        |> should be True

    let checkFunctions f g =
        check f
        check (g 0)
        check (g 1000)

    checkFunctions (Stack.pop) (Stack.popAt)
    checkFunctions (Stack.peek) (Stack.peekAt)



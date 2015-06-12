module HiTop.VM.Tests.``A stack``

open System
open Xunit
open FsUnit.Xunit
open HiTop.VM
open HiTop.VM.Stack

let random = new Random()

let nextRandomByte () =
    random.Next(int Byte.MinValue, int Byte.MaxValue)
    |> byte

/// pushes `n` number of random bytes onto the stack
let pushrandn n stack =
    let rec f (results: ResizeArray<byte>) = function
        | 0 -> results
        | n ->
            let x = nextRandomByte ()

            stack |> WrappedStack.push x

            // The last item on the stack is index 0
            results.Insert(0, x)

            f results (n - 1)

    let results = new ResizeArray<byte>()
    f results n

[<Fact>]
let ``when created should be empty.`` () =
    let s = Stack.create ()
    s |> Stack.isEmpty |> should be True
    s |> Stack.notEmpty |> should be False
    s |> Stack.count |> should equal 0
    s |> fun s -> s.Count |> should equal 0

[<Fact>]
let ``when pushed and popped completely should be empty.`` () =
    let s = Stack.create ()
    
    s |> WrappedStack.push 10uy
    s |> WrappedStack.pop |> ignore
    
    s |> Stack.count |> should equal 0

[<Fact>]
let ``when dropping an element should work the same as poping an element.`` () =
    let s = Stack.create ()

    s |> WrappedStack.push 10uy
    s |> WrappedStack.push 20uy
    s |> WrappedStack.push 30uy

    s |> WrappedStack.pop |> ignore
    s |> WrappedStack.drop
    s |> WrappedStack.dropn 1

    s |> Stack.isEmpty |> should be True

[<Fact>]
let ``when dropping elements should only drop the specified number of elements.`` () =
    let s = Stack.create ()

    // 10 - 4 = 6
    s |> pushrandn 10 |> ignore
    s |> Stack.count |> should equal 10
    s |> WrappedStack.dropn 4
    s |> Stack.count |> should equal 6

    // 6 - 6 = 0
    s |> WrappedStack.dropn 6
    s |> Stack.count |> should equal 0

[<Fact>]
let ``when dropping elements should never fail or go out of bounds.`` () =
    let s = Stack.create ()
    s |> WrappedStack.dropn 1000
    // Should not throw

[<Fact>]
let ``when dropping a negative or 0 number of elements should never fail or go out of founds.`` () =
    let s = Stack.create ()
    s |> pushrandn 10 |> ignore
    
    s |> WrappedStack.dropn 0
    s |> Stack.count |> should equal 10

    s |> WrappedStack.dropn -90
    s |> Stack.count |> should equal 10

[<Fact>]
let ``when pushed and poped should be in the correct order.`` () =
    let s = Stack.create ()

    s |> WrappedStack.push 3uy
    s |> WrappedStack.push 2uy
    s |> WrappedStack.push 1uy

    let popShouldEqual x =
        s
        |> WrappedStack.pop
        |> should equal x

    [1uy; 2uy; 3uy]
    |> List.iter popShouldEqual

    s |> Stack.isEmpty |> should be True

[<Fact>]
let ``when peeked should return the correct value without removing any values from the stack.`` () =
    let s = Stack.create ()

    s |> WrappedStack.push 3uy
    s |> WrappedStack.push 2uy
    s |> WrappedStack.push 1uy

    s
    |> WrappedStack.peekAt 2
    |> should equal 3uy

    s
    |> WrappedStack.peekAt 1
    |> should equal 2uy

    s
    |> WrappedStack.peekAt 0
    |> should equal 1uy

    s
    |> WrappedStack.peek
    |> should equal 1uy

    s |> Stack.notEmpty |> should be True

[<Fact>]
let ``when pushed at 0 index and poped at an index, the values should be the same.`` () =
    let s = Stack.create ()

    s |> WrappedStack.pushAt 0 3uy
    s |> WrappedStack.pushAt 0 2uy
    s |> WrappedStack.pushAt 0 1uy

    s
    |> WrappedStack.popAt 2
    |> should equal 3uy

    s
    |> WrappedStack.popAt 1
    |> should equal 2uy

    s
    |> WrappedStack.popAt 0
    |> should equal 1uy

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

    checkFunctions (WrappedStack.pop) (WrappedStack.popAt)
    checkFunctions (WrappedStack.peek) (WrappedStack.peekAt)

/// returns a value from (n..MaxValue)
let randomWrappingIndex n =
    random.Next(n, Int32.MaxValue)

let testIndexWrapping f =
    let s = Stack.create ()
    let n = random.Next(0, 100)

    let values = s |> pushrandn n
    let i = randomWrappingIndex n

    s
    |> WrappedStack.peekAt i
    |> should equal (values.[i % n])

[<Fact>]
let ``when peeking at an index, the operation should support wrap arround correctly.`` () =
    testIndexWrapping WrappedStack.peekAt

[<Fact>]
let ``when popping from an index, the operation should support wrap arround correctly.`` () =
    testIndexWrapping WrappedStack.popAt

[<Fact>]
let ``when dropping from an index, the operation should support wrap arround correctly.`` () =
    let s = Stack.create ()
    let n = random.Next(0, 100)

    let values = s |> pushrandn n
    let i = randomWrappingIndex n

    s |> WrappedStack.dropAt i

    s |> WrappedStack.peekAt i |> should equal (values.[i % n])
    s |> Stack.count |> should equal (n - 1)

[<Fact>]
let ``when pushing at an index, the operation should support wrap arround correctly.`` () =
    let s = Stack.create ()
    let n = random.Next(0, 100)

    let values = s |> pushrandn n

    let i = randomWrappingIndex n
    let x = nextRandomByte ()

    s |> WrappedStack.pushAt i x

    // You have to add 1 (one) to each wrap around since the size the stack just grew to get to the
    // same position otherwise `i % n` will be off
    s |> WrappedStack.peekAt (i + ((i / n) * 1)) |> should equal x
    s |> Stack.count |> should equal (n + 1)
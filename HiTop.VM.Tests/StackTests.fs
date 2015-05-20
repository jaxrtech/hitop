module HiTop.VM.Tests.``A stack``

open System
open Xunit
open FsUnit.Xunit
open HiTop.VM
open HiTop.VM.CoreTypes

let random = new Random()

let shouldEqualAsValue x y =
    equalAsOptValue (Value(x)) y
    |> should be True

let shouldNotEqualAsValue x y =
    equalAsOptValue (Value(x)) y
    |> should be False

let nextRandomByte () =
    random.Next(int Byte.MinValue, int Byte.MaxValue)
    |> byte

/// pushes `n` number of random bytes onto the stack
let pushrandn n stack =
    let rec f (results: ResizeArray<byte>) = function
        | 0 -> results
        | n ->
            let x = nextRandomByte ()

            stack |> Stack.push (Value(x))

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
    
    s |> Stack.push (Value(10uy))
    s |> Stack.pop |> ignore
    
    s |> Stack.count |> should equal 0

[<Fact>]
let ``when dropping an element should work the same as poping an element.`` () =
    let s = Stack.create ()

    s |> Stack.push (Value(10uy))
    s |> Stack.push (Value(20uy))
    s |> Stack.push (Value(30uy))

    s |> Stack.pop |> ignore
    s |> Stack.drop
    s |> Stack.dropn 1

    s |> Stack.isEmpty |> should be True

[<Fact>]
let ``when dropping elements should only drop the specified number of elements.`` () =
    let s = Stack.create ()

    // 10 - 4 = 6
    s |> pushrandn 10 |> ignore
    s |> Stack.count |> should equal 10
    s |> Stack.dropn 4
    s |> Stack.count |> should equal 6

    // 6 - 6 = 0
    s |> Stack.dropn 6
    s |> Stack.count |> should equal 0

[<Fact>]
let ``when dropping elements should never fail or go out of bounds.`` () =
    let s = Stack.create ()
    s |> Stack.dropn 1000
    // Should not throw

[<Fact>]
let ``when dropping a negative or 0 number of elements should never fail or go out of founds.`` () =
    let s = Stack.create ()
    s |> pushrandn 10 |> ignore
    
    s |> Stack.dropn 0
    s |> Stack.count |> should equal 10

    s |> Stack.dropn -90
    s |> Stack.count |> should equal 10

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

/// returns a value from (n..MaxValue)
let randomWrappingIndex n =
    random.Next(n, Int32.MaxValue)

let testIndexWrapping f =
    let s = Stack.create ()
    let n = random.Next(0, 100)

    let values = s |> pushrandn n
    let i = randomWrappingIndex n

    s
    |> Stack.peekAt i
    |> shouldEqualAsValue (values.[i % n])

[<Fact>]
let ``when peeking at an index, the operation should support wrap arround correctly.`` () =
    testIndexWrapping Stack.peekAt

[<Fact>]
let ``when popping from an index, the operation should support wrap arround correctly.`` () =
    testIndexWrapping Stack.popAt

[<Fact>]
let ``when dropping from an index, the operation should support wrap arround correctly.`` () =
    let s = Stack.create ()
    let n = random.Next(0, 100)

    let values = s |> pushrandn n
    let i = randomWrappingIndex n

    s |> Stack.dropAt i

    s |> Stack.peekAt i |> shouldNotEqualAsValue (values.[i % n])
    s |> Stack.count |> should equal (n - 1)

[<Fact>]
let ``when pushing at an index, the operation should support wrap arround correctly.`` () =
    let s = Stack.create ()
    let n = random.Next(0, 100)

    let values = s |> pushrandn n

    let i = randomWrappingIndex n
    let x = nextRandomByte ()

    s |> Stack.pushAt i (Value(x))

    // You have to add 1 (one) to each wrap around since the size the stack just grew to get to the
    // same position otherwise `i % n` will be off
    s |> Stack.peekAt (i + ((i / n) * 1)) |> shouldEqualAsValue x
    s |> Stack.count |> should equal (n + 1)
module HiTop.VM.Engine

open System.IO
open HiTop.VM.CoreTypes
open HiTop.VM.Stack

let createFromStream (stream: Stream) instructionSet =
    stream.Position <- 0L

    { IsHalted = false
      Cycles = 0UL
      InstructionSet = instructionSet
      Stack = Stack.create ()
      Program = new BinaryReader(stream)
      LastOutput = None
      LastOperation = None }

let createFromBuffer (buffer: byte array) instructionSet =
    let stream = new MemoryStream(buffer)

    createFromStream stream instructionSet

let dispose (engine: Engine) =
    engine.Program.Close()

let nextReadAddress (engine: Engine) =
    engine.Program.BaseStream.Position

let programLength (engine: Engine) =
    engine.Program.BaseStream.Length

let private isPositionInProgram (engine: Engine) (address: int64) =
    match address with
    | x when x < 0L -> false
    | x when x > (engine.Program.BaseStream.Length - 1L) -> false
    | _ -> true

let isEndOfProgram (engine: Engine) : bool =
    (nextReadAddress engine) >= (programLength engine)

let private incrementCyles (engine: Engine) =
    { engine with Cycles = engine.Cycles + 1UL }

let setNextReadAddress (address: int64) (engine: Engine) =
    // Still don't understand why on earth the position in the stream is a signed integer
    // Probably because C# does not have optional types so they had to have `-1` while blowing off
    // half the addressing space of an unsigned 64-bit integer. Seriously.

    let address' =
        if address < 0L
        then 0L
        else address

    engine.Program.BaseStream.Position <- address'
    engine

let private incrementNextReadAddress (engine: Engine) =
    engine.Program.BaseStream.Seek(+1L, SeekOrigin.Current) |> ignore
    engine

let private withLastOperation (op: StepResult) (engine: Engine) =
    { engine with LastOperation = Some(op) }

let private withNoLastOperation (engine: Engine) =
    { engine with LastOperation = None}

let private withNoLastOutput (engine: Engine) =
    { engine with LastOutput = None }

let private markAsHalted (engine: Engine) =
    { engine with IsHalted = true }

//

type private EngineTransformer = Engine -> Engine

//

let private interpret engine raw =
    engine.InstructionSet.FromByte.[raw]

//

type private ExecutionContext = {
    preApply: EngineTransformer
    postApply: EngineTransformer
}

type private ExecutionState =
     | Halted of Engine
     | Running of Engine

[<AutoOpen>]
module private Execution =
    let checkForEndOfProgram engine =
        if isEndOfProgram engine then
            engine
            |> markAsHalted
            |> withNoLastOperation
            |> Halted
        else
            engine
            |> Running
    
    let executeInContextWith f context (engine: Engine) : Engine =
        engine
        |> context.preApply
        |> checkForEndOfProgram
        |> function
           | Running engine' -> f engine'
           | Halted engine' -> engine'
        |> context.postApply

    let private rootExecutionContext =
        let preApply =
            withNoLastOutput

        let postApply =
            incrementNextReadAddress
            >> incrementCyles
            
        { preApply = preApply
          postApply = postApply }
    
    let private readNextExecutionContext =
        let preApply = incrementNextReadAddress

        let postApply = incrementCyles

        { preApply = preApply
          postApply = postApply }

    let rootExecuteWith f =
        let f' engine =
            let raw = engine.Program.ReadByte()
            let insr = raw |> interpret engine
            f engine raw insr

        executeInContextWith f' rootExecutionContext
    
    let private readNextWithInternal f =
        executeInContextWith f readNextExecutionContext

    let readNextWith f = 
        let f' engine =
            let raw = engine.Program.ReadByte()
            f engine raw

        readNextWithInternal f'

    let interpretNextWith f =
        let f' engine =
            let raw = engine.Program.ReadByte()
            let insr = raw |> interpret engine
            f engine insr

        readNextWithInternal f'
//

let private push engine value =
    engine.Stack |> StrictStack.push value
    engine

let private pushFailedOperation raw op engine =
    let name =
        match op with
        | Instruction(insr) -> insr.ShortName
        | op -> sprintf "%A" op

    raw
    |> push engine
    |> withLastOperation (PushedFailedOperation(name))

type private ScanContext = {
    loopPredicate: StackElement -> bool
    markerPredicate: ByteCode -> bool
    stepPosition: int64 -> int64

    ResultWhenMarkerFound: StepResult
    ResultWhenPredicateFailed: StepResult
}

let private scanForMarker engine raw op context =
    let head = engine.Stack |> StrictStack.peek
    match head with
    | Some(x) when context.loopPredicate x -> // while (x) { }
        // Since everything needs defined behavior, if we do not find a
        // matching loop end marker, we will follow suit with other failed operations
        // and simply push the raw value on the stack, skipping the instruction

        let markerAddress = engine |> nextReadAddress |> (-) 1L

        // TODO: A table with the jump addresses would make this O(1) vs this which
        //       worst case will scan the entire program for the end of the loop.
        let rec loop engine =
            engine
            |> interpretNextWith (fun engine' insr ->
                // TODO: And this is why working with nested discriminated unions are an
                //       absolute pain to deal with
                let isMatch = function
                    | x when context.markerPredicate x -> true
                    | _ -> false

                // The current program address is at the current byte that was
                // interpreted. See the execution context that `interpretNextWith`
                // uses if you want to be sure.
                let nextPosition = context.stepPosition (engine' |> nextReadAddress)

                let isLoopMarkerFound = insr |> isMatch

                let isNextPositionInBounds =
                    nextPosition |> isPositionInProgram engine

                if isLoopMarkerFound then
                    // We just found the loop end marker. Jump to the position past that
                    engine'
                    |> incrementNextReadAddress
                    |> withLastOperation context.ResultWhenMarkerFound

                else if isNextPositionInBounds |> not then
                    // If we reached the end of the program and did not find the end
                    // marker follow suit with other "failed" operations by pushing
                    // the raw instruction byte on the stack.
                                
                    // Additionally make sure the next is in the correct position.
                    engine'
                    |> setNextReadAddress (markerAddress + 1L)
                    |> pushFailedOperation raw op
                            
                else
                    // Otherwise, continue scanning for the loop marker
                    engine'
                    |> setNextReadAddress nextPosition
                    |> loop)

        loop engine

    | Some(_) ->
        engine
        |> withLastOperation context.ResultWhenPredicateFailed
                
    | None ->
        // Act just like instructions and push the raw value on the stack since we don't
        // have enough "arguments"
        engine |> pushFailedOperation raw op

let step (engine: Engine) : Engine =
    engine
    |> rootExecuteWith (fun engine raw -> function
        | Value(raw) ->
            raw
            |> push engine
            |> withLastOperation (PushedUnencodedByte(raw))
      
        | EncodedByteMarker ->
            engine
            |> readNextWith (fun engine' raw' ->
                raw'
                |> push engine'
                |> withLastOperation (PushedEncodedByte(raw')))

        | LoopBeginMarker as op ->
            scanForMarker engine raw op {
                loopPredicate =
                    // Match for false since if we had
                    //   while (false) { ... }
                    //
                    // We are going to jump to the end of the loop so we need to find the end
                    // marker
                    (=) hitop_false

                markerPredicate = function
                    | LoopEndMarker -> true
                    | _ -> false

                stepPosition =
                    (+) 1L

                ResultWhenMarkerFound = JumpedToLoopEndMarker
                ResultWhenPredicateFailed = ContinuedAfterLoopBeginMarker
            }

        | LoopEndMarker as op ->
            scanForMarker engine raw op {
                loopPredicate =
                    // Match for true since if we had
                    //   while (true) { ... }
                    //
                    // We are going to jump to the beginning of the loop so we need to find the
                    // end marker
                    (<>) hitop_false

                markerPredicate = function
                    | LoopBeginMarker -> true
                    | _ -> false

                stepPosition =
                    (-) 1L

                ResultWhenMarkerFound = JumpedToLoopBeginMarker
                ResultWhenPredicateFailed = ContinuedAfterLoopEndMarker
            }

        | Instruction op as bytecode ->
            let result = op.Op engine
            match result with
            | Some(engine') ->
                engine'
                |> withLastOperation (ExecutedOperation(op.ShortName))

            | None ->
                // The operation does not have sufficient arguments and will just be converted
                // to the raw byte value and pushed on the stack
                engine |> pushFailedOperation raw bytecode)
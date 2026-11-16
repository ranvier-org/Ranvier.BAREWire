module Ranvier.BAREWire.Tests.SessionTests

open System
open System.Threading
open System.Threading.Tasks
open Expecto
open BAREWire.Framing
open Ranvier.BAREWire
open Ranvier.BAREWire.Tests.Model
open Ranvier.BAREWire.Tests.Support

let private frameError (error: exn) : string =
    match error with
    | :? BareWireException as bare ->
        match bare.Error with
        | BareError.Frame message -> message
        | other -> failtestf "expected a Frame error, got %A" other
    | other -> failtestf "expected BareWireException, got %s" (other.GetType().Name)

/// <summary>A duplex whose transport records what was sent.</summary>
let private recordingDuplex () =
    let sent = ResizeArray<Frame>()

    let duplex =
        BareDuplex (fun bytes _ ->
            lock sent (fun () -> sent.Add (ok (BareFrame.decode bytes)))
            Task.CompletedTask)

    duplex, sent

[<Tests>]
let frames =
    testList
        "frames"
        [
            testCase "a frame is BAREWire's envelope: kind, correlation, payload"
            <| fun () ->
                let encoded = BareFrame.encode (BareFrame.ask 0x01020304u BareCodec.string "hi")

                Expect.equal encoded [| 1uy; 4uy; 3uy; 2uy; 1uy; 2uy; 0x68uy; 0x69uy |] "Ask, correlation little-endian, str"

                let frame = ok (BareFrame.decode encoded)
                Expect.equal frame.Kind FrameKind.Ask "kind"
                Expect.equal frame.Correlation 0x01020304u "correlation"
                Expect.equal (BareFrame.payload BareCodec.string frame) (Ok "hi") "payload"

            testCase "bytes that are no frame are a Frame error"
            <| fun () ->
                let isFrameError result =
                    match result with
                    | Error (BareError.Frame _) -> true
                    | _ -> false

                Expect.isTrue (isFrameError (BareFrame.decode null)) "null"
                Expect.isTrue (isFrameError (BareFrame.decode [| 1uy; 0uy; 0uy |])) "a short header"
                Expect.isTrue (isFrameError (BareFrame.decode [| 9uy; 0uy; 0uy; 0uy; 0uy |])) "an unknown kind byte"
        ]

[<Tests>]
let client =
    testList
        "BareClient"
        [
            testCaseAsync "an ask completes with its reply, under a fresh correlation each time"
            <| async {
                let correlations = ResizeArray<uint32>()

                let bare =
                    BareClient.OverBytes (peer correlations BareCodec.string person (fun name -> { ada with Name = name }))

                let! first =
                    bare.Ask (BareCodec.string, person, "Grace", CancellationToken.None)
                    |> Async.AwaitTask

                let! second =
                    bare.Ask (BareCodec.string, person, "Edsger", CancellationToken.None)
                    |> Async.AwaitTask

                Expect.equal first.Name "Grace" "first reply"
                Expect.equal second.Name "Edsger" "second reply"
                Expect.equal (List.ofSeq correlations) [ 1u; 2u ] "correlation ids start at 1 and never repeat 0"
            }

            testCase "a reply to another correlation, or a frame that is no reply, faults the ask"
            <| fun () ->
                let answering (answer: Frame -> Frame) =
                    BareClient (fun asked _ -> Task.FromResult (answer asked))

                let stale =
                    answering (fun asked -> BareFrame.reply (asked.Correlation + 5u) BareCodec.u8 1uy)

                Expect.stringContains
                    (frameError (faultOf (stale.Ask (BareCodec.u8, BareCodec.u8, 0uy, CancellationToken.None))))
                    "answered by the Reply to correlation 6"
                    "names both correlations"

                let pushed = answering (fun _ -> BareFrame.event BareCodec.u8 1uy)

                Expect.stringContains
                    (frameError (faultOf (pushed.Ask (BareCodec.u8, BareCodec.u8, 0uy, CancellationToken.None))))
                    "answered by a Event frame"
                    "names the kind"

            testCase "a reply payload that does not decode faults the ask with the codec's error"
            <| fun () ->
                let bare =
                    BareClient (fun asked _ -> Task.FromResult (Envelope.reply asked.Correlation [| 5uy; 0x68uy |]))

                match faultOf (bare.Ask (BareCodec.u8, BareCodec.string, 0uy, CancellationToken.None)) with
                | :? BareWireException as bare ->
                    match bare.Error with
                    | BareError.Malformed _ -> ()
                    | other -> failtestf "expected Malformed, got %A" other
                | other -> failtestf "expected BareWireException, got %s" (other.GetType().Name)

            testCase "a request the codec cannot hold faults the ask before anything is sent"
            <| fun () ->
                let mutable sent = 0

                let bare =
                    BareClient (fun asked _ ->
                        sent <- sent + 1
                        Task.FromResult asked)

                let error =
                    faultOf (bare.Ask (BareCodec.string, BareCodec.u8, null, CancellationToken.None))

                Expect.isTrue (error :? ArgumentException) "ArgumentException"
                Expect.equal sent 0 "nothing sent"

            testCaseAsync "an asker over a union protocol builds the ask and takes the awaited case"
            <| async {
                let correlations = ResizeArray<uint32>()

                let bare =
                    BareClient.OverBytes (peer correlations BareCodec.f64 shape (fun radius -> if radius > 0.0 then Circle radius else Empty))

                let area =
                    bare.Asker (
                        BareCodec.f64,
                        shape,
                        (fun (diameter: float) -> diameter / 2.0),
                        (function
                        | Circle radius -> Math.PI * radius * radius
                        | other -> failwithf "unexpected %A" other)
                    )

                let! computed = area 2.0 CancellationToken.None |> Async.AwaitTask
                Expect.floatClose Accuracy.high computed Math.PI "the awaited case"
                Expect.equal (faultOf (area 0.0 CancellationToken.None)).Message "unexpected Empty" "any other case raises"
            }
        ]

[<Tests>]
let duplex =
    testList
        "BareDuplex"
        [
            testCaseAsync "replies arriving out of order complete their own asks"
            <| async {
                let duplex, sent = recordingDuplex ()

                let bare =
                    BareClient (fun frame cancellation -> duplex.Exchange (frame, cancellation))

                let first =
                    bare.Ask (BareCodec.string, BareCodec.string, "one", CancellationToken.None)

                let second =
                    bare.Ask (BareCodec.string, BareCodec.string, "two", CancellationToken.None)

                Expect.equal (sent |> Seq.map _.Kind |> List.ofSeq) [ FrameKind.Ask; FrameKind.Ask ] "two asks sent"

                let answer (index: int) (text: string) =
                    duplex.Receive (BareFrame.encode (BareFrame.reply sent.[index].Correlation BareCodec.string text))

                Expect.equal (answer 1 "second") (Ok ()) "the later ask is answered first"
                Expect.equal (answer 0 "first") (Ok ()) "then the earlier one"
                let! one = Async.AwaitTask first
                let! two = Async.AwaitTask second
                Expect.equal (one, two) ("first", "second") "each ask has its own reply"
            }

            testCase "a reply no ask awaits is a Frame error"
            <| fun () ->
                let duplex, _ = recordingDuplex ()

                match duplex.Receive (BareFrame.encode (BareFrame.reply 9u BareCodec.u8 1uy)) with
                | Error (BareError.Frame message) -> Expect.stringContains message "correlation 9" "names the correlation"
                | other -> failtestf "expected a Frame error, got %A" other

            testCase "events reach their subscribers until the subscription is disposed"
            <| fun () ->
                let duplex, _ = recordingDuplex ()
                let received = ResizeArray<Result<Colour, BareError>>()
                let subscription = duplex.OnEvent (colour, received.Add)

                let push (frame: Frame) =
                    duplex.Receive (BareFrame.encode frame) |> ok

                push (BareFrame.event colour Green)
                push (BareFrame.tell colour Red)
                push (Envelope.event [| 9uy |])
                subscription.Dispose ()
                push (BareFrame.event colour Blue)

                match List.ofSeq received with
                | [ Ok Green; Error (BareError.Malformed _) ] -> ()
                | other -> failtestf "expected the event and the undecodable event only, got %A" other

            testCase "a tell is sent as a Tell frame"
            <| fun () ->
                let duplex, sent = recordingDuplex ()
                duplex.Tell(colour, Blue, CancellationToken.None).GetAwaiter().GetResult()
                Expect.equal sent.[0].Kind FrameKind.Tell "kind"
                Expect.equal (BareFrame.payload colour sent.[0]) (Ok Blue) "payload"

            testCase "closing faults the waiting asks and refuses later ones"
            <| fun () ->
                let duplex, _ = recordingDuplex ()

                let bare =
                    BareClient (fun frame cancellation -> duplex.Exchange (frame, cancellation))

                let waiting = bare.Ask (BareCodec.u8, BareCodec.u8, 1uy, CancellationToken.None)
                duplex.Close (TimeoutException "socket closed")
                Expect.equal (faultOf waiting).Message "socket closed" "the waiting ask"

                Expect.equal (faultOf (bare.Ask (BareCodec.u8, BareCodec.u8, 2uy, CancellationToken.None))).Message "socket closed" "a later ask"

            testCase "cancelling the token cancels the waiting ask and forgets its correlation"
            <| fun () ->
                let duplex, sent = recordingDuplex ()

                let bare =
                    BareClient (fun frame cancellation -> duplex.Exchange (frame, cancellation))

                use cancellation = new CancellationTokenSource ()
                let waiting = bare.Ask (BareCodec.u8, BareCodec.u8, 1uy, cancellation.Token)
                cancellation.Cancel ()
                Expect.isTrue ((faultOf waiting) :? OperationCanceledException) "cancelled"

                match duplex.Receive (BareFrame.encode (BareFrame.reply sent.[0].Correlation BareCodec.u8 1uy)) with
                | Error (BareError.Frame _) -> ()
                | other -> failtestf "expected the late reply to be refused, got %A" other

            testCase "a failed send faults the ask and forgets its correlation"
            <| fun () ->
                let duplex =
                    BareDuplex (fun _ _ -> Task.FromException (InvalidOperationException "not connected"))

                let error =
                    faultOf (duplex.Exchange (BareFrame.ask 3u BareCodec.u8 1uy, CancellationToken.None))

                Expect.equal error.Message "not connected" "the transport's failure"

                match duplex.Receive (BareFrame.encode (BareFrame.reply 3u BareCodec.u8 1uy)) with
                | Error (BareError.Frame _) -> ()
                | other -> failtestf "expected no waiting ask, got %A" other

            testCase "only an Ask awaits a reply, and one correlation waits once"
            <| fun () ->
                let duplex, _ = recordingDuplex ()

                Expect.stringContains
                    (frameError (faultOf (duplex.Exchange (BareFrame.tell BareCodec.u8 1uy, CancellationToken.None))))
                    "Tell frame"
                    "a Tell is refused"

                let _first =
                    duplex.Exchange (BareFrame.ask 4u BareCodec.u8 1uy, CancellationToken.None)

                Expect.stringContains
                    (frameError (faultOf (duplex.Exchange (BareFrame.ask 4u BareCodec.u8 1uy, CancellationToken.None))))
                    "correlation 4 is already waiting"
                    "a repeated correlation is refused"
        ]

namespace Ranvier.BAREWire

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open BAREWire.Framing

/// <summary>
/// Asks over BAREWire frames: each ask is an Ask frame with a fresh correlation id, answered by the Reply frame
/// that echoes it.
/// </summary>
/// <remarks>
/// <c>exchange</c> carries one Ask frame to the peer and completes with the frame that answers it. A transport that
/// pairs a request with its response, as an HTTP POST, supplies it through <c>BareClient.OverBytes</c>; a message
/// transport, as a WebSocket, supplies <c>BareDuplex.Exchange</c>.
/// </remarks>
[<Sealed>]
type BareClient(exchange: Frame -> CancellationToken -> Task<Frame>) =
    let gate = obj ()
    let mutable last = 0u

    let nextCorrelation () : uint32 =
        lock gate (fun () ->
            // 0 marks a frame that carries no correlation, so the counter skips it when it wraps.
            last <- (if last = UInt32.MaxValue then 1u else last + 1u)
            last)

    /// <summary>A client over a transport that returns the bytes of the answering frame for the bytes of an Ask.</summary>
    static member OverBytes(exchange: byte array -> CancellationToken -> Task<byte array>) : BareClient =
        BareClient (fun frame cancellation ->
            task {
                let! answer = exchange (BareFrame.encode frame) cancellation

                return
                    match BareFrame.decode answer with
                    | Ok decoded -> decoded
                    | Error error -> raise (BareWireException error)
            })

    /// <summary>
    /// Sends the value as an Ask and completes with the Reply's value. The task faults with
    /// <c>ArgumentException</c> for a value the request codec cannot hold, with
    /// <see cref="T:Ranvier.BAREWire.BareWireException"/> for an answer that is not the awaited Reply or whose
    /// payload does not decode, and with the transport's own failure.
    /// </summary>
    member _.Ask(request: BareCodec<'Request>, reply: BareCodec<'Reply>, value: 'Request, cancellation: CancellationToken) : Task<'Reply> =
        task {
            let correlation = nextCorrelation ()
            let! answer = exchange (BareFrame.ask correlation request value) cancellation

            if answer.Kind <> FrameKind.Reply then
                raise (
                    BareWireException (
                        BareError.Frame (
                            "The ask with correlation "
                            + string correlation
                            + " was answered by a "
                            + BareFrame.kindName answer.Kind
                            + " frame."
                        )
                    )
                )

            if answer.Correlation <> correlation then
                raise (
                    BareWireException (
                        BareError.Frame (
                            "The ask with correlation "
                            + string correlation
                            + " was answered by the Reply to correlation "
                            + string answer.Correlation
                            + "."
                        )
                    )
                )

            return
                match reply.Decode answer.Payload with
                | Ok decoded -> decoded
                | Error error -> raise (BareWireException error)
        }

    /// <summary>
    /// <c>Ask</c> as a function of the request and a cancellation token: the shape of a Ranvier.Query loader and of
    /// a mutation's remote write.
    /// </summary>
    member this.Asker(request: BareCodec<'Request>, reply: BareCodec<'Reply>) : 'Request -> CancellationToken -> Task<'Reply> =
        fun value cancellation -> this.Ask (request, reply, value, cancellation)

    /// <summary>
    /// As <c>Asker</c>, for a protocol whose asks and replies are each one union: <c>toRequest</c> builds the ask
    /// from a key, and <c>ofReply</c> takes the awaited case from the reply, raising for any other.
    /// </summary>
    member this.Asker
        (request: BareCodec<'Request>, reply: BareCodec<'Reply>, toRequest: 'Key -> 'Request, ofReply: 'Reply -> 'T)
        : 'Key -> CancellationToken -> Task<'T> =
        fun key cancellation ->
            task {
                let! answer = this.Ask (request, reply, toRequest key, cancellation)
                return ofReply answer
            }

/// <summary>
/// Correlates frames over a message transport: an Ask sent through <c>Exchange</c> completes when <c>Receive</c> is
/// given its Reply, and every other frame received goes to the subscribers.
/// </summary>
/// <remarks>
/// <c>send</c> hands one frame's bytes to the transport. The host calls <c>Receive</c> with each message that
/// arrives, from any thread. Subscribers run on the thread that calls <c>Receive</c>.
/// </remarks>
[<Sealed>]
type BareDuplex(send: byte array -> CancellationToken -> Task) =
    let gate = obj ()
    let pending = Dictionary<uint32, TaskCompletionSource<Frame>>()
    let mutable subscribers: (obj * (Frame -> unit)) array = Array.empty
    let mutable closed: exn = null

    let take (correlation: uint32) : TaskCompletionSource<Frame> =
        lock gate (fun () ->
            match pending.TryGetValue correlation with
            | true, source ->
                pending.Remove correlation |> ignore
                source
            | false, _ -> null)

    /// <summary>
    /// Sends an Ask frame and completes with its Reply. The task faults when the frame is not an Ask, when an ask
    /// with its correlation id is already waiting, when sending fails, and with the reason given to <c>Close</c>;
    /// on .NET it is cancelled with the token.
    /// </summary>
    member _.Exchange(frame: Frame, cancellation: CancellationToken) : Task<Frame> =
        task {
            if frame.Kind <> FrameKind.Ask then
                raise (
                    BareWireException (
                        BareError.Frame (
                            "Only an Ask frame awaits a Reply; this is a "
                            + BareFrame.kindName frame.Kind
                            + " frame."
                        )
                    )
                )

#if FABLE_COMPILER
            let source = TaskCompletionSource<Frame>()
#else
            let source =
                TaskCompletionSource<Frame> TaskCreationOptions.RunContinuationsAsynchronously
#endif

            lock gate (fun () ->
                if not (isNull closed) then
                    raise closed

                if pending.ContainsKey frame.Correlation then
                    raise (
                        BareWireException (
                            BareError.Frame (
                                "An ask with correlation "
                                + string frame.Correlation
                                + " is already waiting."
                            )
                        )
                    )

                pending.Add (frame.Correlation, source))

#if !FABLE_COMPILER
            use _registration =
                cancellation.Register (fun () ->
                    match take frame.Correlation with
                    | null -> ()
                    | waiting -> waiting.TrySetCanceled cancellation |> ignore)
#endif

            try
                do! send (BareFrame.encode frame) cancellation
            with error ->
                take frame.Correlation |> ignore
                raise error

            return! source.Task
        }

    /// <summary>
    /// Takes one message from the transport. A Reply completes the ask that awaits it; any other frame goes to each
    /// subscriber. <c>BareError.Frame</c> for bytes that are no frame and for a Reply no ask awaits.
    /// </summary>
    member _.Receive(bytes: byte array) : Result<unit, BareError> =
        match BareFrame.decode bytes with
        | Error error -> Error error
        | Ok frame ->
            if frame.Kind = FrameKind.Reply then
                match take frame.Correlation with
                | null ->
                    Error (
                        BareError.Frame (
                            "A Reply arrived for correlation "
                            + string frame.Correlation
                            + ", which no ask awaits."
                        )
                    )
                | waiting ->
                    waiting.SetResult frame
                    Ok ()
            else
                let current = lock gate (fun () -> subscribers)

                for _, handler in current do
                    handler frame

                Ok ()

    /// <summary>Calls <c>handler</c> with each frame received that is not a Reply, until the result is disposed.</summary>
    member _.Subscribe(handler: Frame -> unit) : IDisposable =
        let token = obj ()
        lock gate (fun () -> subscribers <- Array.append subscribers [| (token, handler) |])

        { new IDisposable with
            member _.Dispose() =
                lock gate (fun () ->
                    subscribers <-
                        subscribers
                        |> Array.filter (fun (entry, _) -> not (obj.ReferenceEquals (entry, token))))
        }

    /// <summary>
    /// Calls <c>handler</c> with the decoded payload of each Event frame received, or with the reason its payload
    /// did not decode, until the result is disposed.
    /// </summary>
    member this.OnEvent(codec: BareCodec<'Event>, handler: Result<'Event, BareError> -> unit) : IDisposable =
        this.Subscribe (fun frame ->
            if frame.Kind = FrameKind.Event then
                handler (BareFrame.payload codec frame))

    /// <summary>Sends the value as a Tell frame, which awaits no answer.</summary>
    member _.Tell(codec: BareCodec<'T>, value: 'T, cancellation: CancellationToken) : Task =
        send (BareFrame.encode (BareFrame.tell codec value)) cancellation

    /// <summary>
    /// Faults every waiting ask with <c>reason</c> and refuses later asks with it: call it when the transport closes.
    /// </summary>
    member _.Close(reason: exn) : unit =
        let orphaned =
            lock gate (fun () ->
                closed <- reason
                let waiting = Seq.toArray pending.Values
                pending.Clear ()
                waiting)

        for source in orphaned do
            source.SetException reason

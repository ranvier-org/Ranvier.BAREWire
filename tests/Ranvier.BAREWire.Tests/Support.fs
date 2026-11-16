module Ranvier.BAREWire.Tests.Support

open System.Threading
open System.Threading.Tasks
open Expecto
open Ranvier
open Ranvier.BAREWire

/// <summary>An isolated graph for asynchronous tests that resume on different test-runner threads.</summary>
let newGraph () =
    new Graph (
        { GraphOptions.Default with
            ThreadAffinity = ThreadAffinity.Unchecked
            Dispatcher = Some (ImmediateDispatcher ())
        }
    )

/// <summary>Yields until the condition holds, failing after five seconds.</summary>
let waitUntil condition =
    async {
        let mutable attempts = 0

        while not (condition ()) && attempts < 5000 do
            do! Async.Sleep 1
            attempts <- attempts + 1

        Expect.isTrue (condition ()) "the awaited completion was applied"
    }

/// <summary>The value of an <c>Ok</c>, failing the test on an <c>Error</c>.</summary>
let ok (result: Result<'T, BareError>) : 'T =
    match result with
    | Ok value -> value
    | Error error -> failtestf "expected Ok, got %A" error

/// <summary>
/// An in-process peer for <c>BareClient.OverBytes</c>: decodes each Ask, records its correlation id, and answers
/// with the Reply that <c>handle</c> gives.
/// </summary>
let peer
    (correlations: ResizeArray<uint32>)
    (request: BareCodec<'Request>)
    (reply: BareCodec<'Reply>)
    (handle: 'Request -> 'Reply)
    : byte array -> CancellationToken -> Task<byte array> =
    fun bytes _ ->
        let frame = ok (BareFrame.decode bytes)
        lock correlations (fun () -> correlations.Add frame.Correlation)
        let asked = ok (BareFrame.payload request frame)
        Task.FromResult (BareFrame.encode (BareFrame.reply frame.Correlation reply (handle asked)))

/// <summary>The exception a faulted task holds, unwrapped from its aggregate.</summary>
let faultOf (pending: Task<'T>) : exn =
    try
        pending.GetAwaiter().GetResult() |> ignore
        failtest "expected the task to fault"
    with error ->
        error

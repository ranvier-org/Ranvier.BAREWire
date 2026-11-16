module Ranvier.BAREWire.Tests.QueryTests

open System.Threading
open System.Threading.Tasks
open Expecto
open BAREWire.Framing
open Ranvier.Query
open Ranvier.BAREWire
open Ranvier.BAREWire.Query
open Ranvier.BAREWire.Tests.Model
open Ranvier.BAREWire.Tests.Support

type Rename = { Id: uint32; Name: string }

let private rename: BareCodec<Rename> =
    bareStruct {
        let! id = BareCodec.field "id" _.Id BareCodec.uint32
        let! name = BareCodec.field "name" _.Name BareCodec.string
        return { Id = id; Name = name }
    }

[<Tests>]
let tests =
    testList
        "Ranvier.Query over BAREWire"
        [
            testCaseAsync "a family loads its data through a BAREWire ask keyed by the query key"
            <| async {
                use graph = newGraph ()
                use _active = graph.Activate ()
                use client = new QueryClient (graph)
                let correlations = ResizeArray<uint32>()

                let bare =
                    BareClient.OverBytes (peer correlations BareCodec.uint32 person (fun id -> { ada with Age = int id }))

                let people = client.DefineBare (bare, BareCodec.uint32, person)
                use first = people.Acquire 41u
                use second = people.Acquire 42u

                do!
                    waitUntil (fun () ->
                        first.State.Data.IsSome
                        && second.State.Data.IsSome)

                Expect.equal first.State.Data (Some { ada with Age = 41 }) "the first key's data"
                Expect.equal second.State.Data (Some { ada with Age = 42 }) "the second key's data"
                Expect.equal correlations.Count 2 "one ask per key"
                use again = people.Acquire 41u
                Expect.equal correlations.Count 2 "a loaded key is not asked again"
            }

            testCaseAsync "a reply that does not decode is the query's error, and a refresh recovers"
            <| async {
                use graph = newGraph ()
                use _active = graph.Activate ()
                use client = new QueryClient (graph)
                let mutable broken = true

                let bare =
                    BareClient (fun asked _ ->
                        if broken then
                            Task.FromResult (Envelope.reply asked.Correlation [| 0xFFuy |])
                        else
                            Task.FromResult (BareFrame.reply asked.Correlation person ada))

                let people = client.DefineBare (bare, BareCodec.uint32, person)
                use query = people.Acquire 1u
                do! waitUntil (fun () -> query.State.Error.IsSome)

                match query.State.Error with
                | Some (:? BareWireException as error) ->
                    match error.Error with
                    | BareError.Malformed _ -> ()
                    | other -> failtestf "expected Malformed, got %A" other
                | other -> failtestf "expected BareWireException, got %A" other

                broken <- false
                query.Refresh ()
                do! waitUntil (fun () -> query.State.Data.IsSome)
                Expect.equal query.State.Data (Some ada) "the refreshed data"
            }

            testCaseAsync "a family over a union protocol builds asks from keys and takes the awaited case"
            <| async {
                use graph = newGraph ()
                use _active = graph.Activate ()
                use client = new QueryClient (graph)

                let bare =
                    BareClient.OverBytes (peer (ResizeArray ()) BareCodec.f64 shape (fun radius -> Circle radius))

                let radii =
                    client.DefineBare (
                        bare,
                        BareCodec.f64,
                        shape,
                        (fun (diameter: float) -> diameter / 2.0),
                        (function
                        | Circle radius -> radius
                        | other -> failwithf "unexpected %A" other)
                    )

                use query = radii.Acquire 10.0
                do! waitUntil (fun () -> query.State.Data.IsSome)
                Expect.equal query.State.Data (Some 5.0) "the awaited case's value"
            }

            testCaseAsync "a mutation sent as an ask reconciles the loaded query from its reply"
            <| async {
                use graph = newGraph ()
                use _active = graph.Activate ()
                use client = new QueryClient (graph)

                let loads =
                    BareClient.OverBytes (peer (ResizeArray ()) BareCodec.uint32 person (fun _ -> ada))

                let writes = BareClient.OverBytes (peer (ResizeArray ()) rename rename id)
                let people = client.DefineBare (loads, BareCodec.uint32, person)
                use query = people.Acquire 7u
                do! waitUntil (fun () -> query.State.Data.IsSome)

                let pending =
                    client.MutateBare (
                        writes,
                        rename,
                        rename,
                        { Id = 7u; Name = "Augusta" },
                        fun saved -> [ people.UpdateIfLoaded (saved.Id, fun old -> { old with Name = saved.Name }) ]
                    )

                do! waitUntil (fun () -> pending.IsCompleted)

                match pending.Result with
                | MutationOutcome.Applied saved -> Expect.equal saved { Id = 7u; Name = "Augusta" } "the saved receipt"
                | other -> failtestf "expected Applied, got %A" other

                Expect.equal query.State.Data (Some { ada with Name = "Augusta" }) "the reconciled data"
                Expect.isFalse query.State.IsStale "an updated entry is fresh"
            }

            testCaseAsync "a failed remote write is RequestFailed and leaves the cache as it was"
            <| async {
                use graph = newGraph ()
                use _active = graph.Activate ()
                use client = new QueryClient (graph)

                let loads =
                    BareClient.OverBytes (peer (ResizeArray ()) BareCodec.uint32 person (fun _ -> ada))

                let writes =
                    BareClient (fun _ _ -> Task.FromResult (BareFrame.event BareCodec.u8 0uy))

                let people = client.DefineBare (loads, BareCodec.uint32, person)
                use query = people.Acquire 7u
                do! waitUntil (fun () -> query.State.Data.IsSome)

                let pending =
                    client.MutateBare (writes, rename, rename, { Id = 7u; Name = "x" }, fun _ -> [ people.Invalidate 7u ])

                do! waitUntil (fun () -> pending.IsCompleted)

                match pending.Result with
                | MutationOutcome.RequestFailed (:? BareWireException) -> ()
                | other -> failtestf "expected RequestFailed with BareWireException, got %A" other

                Expect.equal query.State.Data (Some ada) "the cached data"
                Expect.isFalse query.State.IsStale "no edit was published"
            }

            testCaseAsync "an event received off the graph's thread invalidates a loaded query on it"
            <| async {
                use graph = newGraph ()
                use _active = graph.Activate ()
                use client = new QueryClient (graph)
                let duplex = BareDuplex (fun _ _ -> Task.CompletedTask)

                let loads =
                    BareClient.OverBytes (peer (ResizeArray ()) BareCodec.uint32 person (fun _ -> ada))

                let people = client.DefineBare (loads, BareCodec.uint32, person)
                use query = people.Acquire 7u
                do! waitUntil (fun () -> query.State.Data.IsSome)
                let failures = ResizeArray<BareError>()

                use _subscription =
                    duplex.OnEvent (
                        graph,
                        BareCodec.uint32,
                        function
                        | Ok changed -> client.Commit [ people.Invalidate changed ]
                        | Error error -> failures.Add error
                    )

                let receive (frame: Frame) =
                    Task.Run (fun () -> duplex.Receive (BareFrame.encode frame))
                    |> Async.AwaitTask

                let! refused = receive (Envelope.event [| 0x80uy |])
                Expect.equal refused (Ok ()) "the frame itself was a frame"
                Expect.equal failures.Count 1 "an undecodable event reaches the handler as an error"
                Expect.isFalse query.State.IsStale "and changes nothing"
                let! _ = receive (BareFrame.event BareCodec.uint32 7u)
                do! waitUntil (fun () -> query.State.IsStale)
                Expect.equal query.State.Data (Some ada) "invalidation keeps the accepted data"
            }
        ]

---
title: Ranvier.Query bindings
description: Bind BAREWire asks, mutations, and duplex server events to Ranvier.Query families and reactive graphs.
---

# Ranvier.Query bindings

`Ranvier.BAREWire.Query` connects `BareClient` and `BareDuplex` from [Codecs and framing](codecs-and-framing.md) to [`Ranvier.Query`](repo:core/guide/queries.md) query families, serialized mutations, and graph-thread cache invalidation.

## Defining query families with `DefineBare`

Opening `Ranvier.BAREWire.Query` brings the `BareQueryExtensions` members on `QueryClient` and `BareDuplex` into scope:

```fsharp
open Ranvier
open Ranvier.Query
open Ranvier.BAREWire
open Ranvier.BAREWire.Query

use graph = new Graph ()
use client = new QueryClient (graph)
let bare = BareClient.OverBytes transport

// Query key is the Ask payload; query value is the Reply payload
let people: QueryFamily<uint32, Person> =
    client.DefineBare (bare, BareCodec.uint32, person)

use lease = people.Acquire 42u
```

`DefineBare` performs no network I/O when called; `Acquire` and `Ensure` express demand on the `QueryFamily<'Key, 'T>` just like `QueryClient.Define`. Keys use the default F# equality comparer.

If a remote call fails, the query transitions to its error state carrying:

- `BareWireException` when the peer returns a non-`Reply` frame, a mismatched correlation ID, a truncated envelope, or a payload that fails `BareCodec.Decode`; or
- the underlying transport exception (such as `HttpRequestException` or socket closure).

### Single-union protocols

When a protocol multiplexes all requests through one BARE union codec and all responses through one reply union codec, use the five-argument `DefineBare` overload to project between the query key/value and the protocol union cases:

```fsharp
let sections =
    client.DefineBare (
        bare,
        Protocol.request,
        Protocol.reply,
        (fun (id: SectionId) -> Request.GetSection id),
        (function
         | Reply.Section section -> section
         | other -> failwithf "Unexpected reply case: %A" other)
    )
```

## Mutations and cache reconciliation with `MutateBare`

`client.MutateBare` queues a serialized remote write over `BareClient` and applies the returned `QueryEdit list` from `reconcile` to the `QueryClient` before the next queued mutation begins:

```fsharp
let savePerson (draft: RenamePerson) =
    client.MutateBare (
        bare,
        renamePersonCodec,
        person,
        draft,
        fun saved ->
            [ people.UpdateIfLoaded (saved.Id, fun _ -> saved) ]
    )
```

`MutateBare` delegates to `QueryClient.Mutate` with `bare.Asker(request, reply)` and returns the same `Task` mutation outcome.

## Graph-thread event delivery with `BareDuplex.OnEvent`

`BareDuplex.Receive` invokes raw subscribers on whichever background I/O thread reads from the socket. `Ranvier.BAREWire.Query` adds an overload `duplex.OnEvent(graph, codec, handler)` that marshals every decoded `Event` frame onto the `Graph`'s owning thread via `graph.Dispatch`:

```fsharp
use subscription =
    duplex.OnEvent (
        graph,
        BareCodec.uint32,
        function
        | Ok changedId ->
            client.Commit [ people.Invalidate changedId ]
        | Error decodeError ->
            eprintfn "Malformed event frame: %s" decodeError.Message
    )
```

When `duplex.Receive` already runs on the graph's thread, `graph.Dispatch` executes inline; from any background receive loop, it schedules the callback through the graph's dispatcher so `client.Commit` and signal writes obey Ranvier's thread-affinity contract. Invalidating a key marks loaded entries stale without eagerly fetching; active leases or the next `Ensure` / `Refresh` trigger the reload.

# Ranvier.BAREWire.Query

[Ranvier.Query](https://www.nuget.org/packages/Ranvier.Query) families and mutations whose remote calls are
BAREWire asks, over the codecs and frames of
[Ranvier.BAREWire](https://www.nuget.org/packages/Ranvier.BAREWire).

```fsharp
open Ranvier.Query
open Ranvier.BAREWire
open Ranvier.BAREWire.Query

use client = new QueryClient (graph)
let bare = BareClient.OverBytes transport

// The query key is the Ask's payload; the data is the Reply's.
let people = client.DefineBare (bare, BareCodec.uint32, person)
use page = people.Acquire 42u

// A remote write sent as an ask, reconciled from its Reply.
let save draft =
    client.MutateBare (bare, rename, person, draft, fun saved ->
        [ people.UpdateIfLoaded (saved.Id, fun _ -> saved) ])
```

`DefineBare` performs no IO; `Acquire` and `Ensure` express demand, as for any query family. A failed ask is the
query's `Error`: a `BareWireException` for an answer that is not the awaited Reply or whose payload does not decode,
and the transport's own failure otherwise. `MutateBare` returns the outcome `QueryClient.Mutate` returns.

For a protocol whose asks and replies are each one union, an overload takes the two conversions:

```fsharp
let sections =
    client.DefineBare (bare, Protocol.request, Protocol.reply,
        (fun (id: SectionId) -> GetSection id),
        (function
         | Section section -> section
         | other -> failwith "expected a section"))
```

## Events

`OnEvent` delivers each Event frame a `BareDuplex` receives on the graph's thread, so the handler may commit query
edits:

```fsharp
use subscription =
    duplex.OnEvent (graph, changedId, function
        | Ok id -> client.Commit [ people.Invalidate id ]
        | Error error -> log error)
```

Invalidation fetches nothing by itself: the next `Ensure` or `Refresh` asks again.

## Targets

.NET 10, .NET Standard 2.1 and Fable. The .NET 10 build is trimmable and NativeAOT-compatible. Use the matching
`Ranvier.BAREWire.Query.Traced` package with a traced graph build.

## Licence

MIT. The package depends on Ranvier.BAREWire, which includes BAREWire under the Apache License 2.0.

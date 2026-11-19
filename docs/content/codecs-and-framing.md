---
title: Codecs and framing
description: Typed BARE binary codecs, schema generation and validation, envelope frames, and correlated request/reply or duplex transports.
---

# Codecs and framing

`Ranvier.BAREWire` pairs BAREWire's zero-allocation cursor readers and writers with exact byte sizing, BARE schema descriptors, and envelope framing.

## `BareCodec<'T>` and `BareError`

A `BareCodec<'T>` combines a BARE `SchemaType`, its referenced `NamedType array` declarations, an exact `Size: 'T -> int` calculation, and cursor-based `Write` and `Read` functions:

```fsharp
open Ranvier.BAREWire

type Colour =
    | Red
    | Green
    | Blue

type Person =
    { Name: string
      Age: int
      Colour: Colour
      Nick: string option }

let colour =
    BareCodec.enum "Colour" [
        "RED", 0UL, Red
        "GREEN", 1UL, Green
        "BLUE", 2UL, Blue
    ]

let person =
    bareStruct {
        let! name = BareCodec.field "name" _.Name BareCodec.string
        let! age = BareCodec.field "age" _.Age BareCodec.int32
        let! colour = BareCodec.field "colour" _.Colour colour
        let! nick = BareCodec.field "nick" _.Nick (BareCodec.option BareCodec.string)
        return { Name = name; Age = age; Colour = colour; Nick = nick }
    }
    |> BareCodec.named "Person"

let bytes: byte array =
    person.Encode { Name = "Ada"; Age = 36; Colour = Blue; Nick = None }

let decoded: Result<Person, BareError> =
    person.Decode bytes
```

- `codec.Encode(value)` allocates an exact-sized `byte array` and writes `value`. Values that cannot be represented in the layout (such as a `null` string or array, a `fixedData` / `fixedArray` of the wrong length, or an undeclared enum/union value) raise `ArgumentException`.
- `codec.Decode(bytes)` requires the entire buffer to be consumed and returns `Result<'T, BareError>` without throwing:
  - `BareError.Malformed message` — `null` input, truncated bytes, invalid `bool` byte, out-of-range varint, duplicate map key, or unknown enum/union tag.
  - `BareError.Trailing message` — a valid value ended before `bytes.Length`.
  - `BareError.Shape message` — a conversion function raised while constructing the decoded value.
  - `BareError.Frame message` — invalid frame header, unknown frame kind, unexpected frame kind, or unmatched correlation ID.
- `codec.TypeText` formats the codec's BARE type expression (for example `Person` or `struct { name: str ... }`).
- Where a `Task` faults instead of returning `Result`, `BareWireException` wraps the underlying `BareError` (`ex.Error`).

## Built-in codecs and combinators

| BARE type | Combinator | F# type |
| --- | --- | --- |
| `uint`, `int` | `BareCodec.uint`, `BareCodec.int` | `uint64`, `int64` |
| `uint`, `int` (32-bit checked) | `BareCodec.uint32`, `BareCodec.int32` | `uint32`, `int` |
| `u8`, `u16`, `u32`, `u64` | `BareCodec.u8`, `BareCodec.u16`, `BareCodec.u32`, `BareCodec.u64` | `byte`, `uint16`, `uint32`, `uint64` |
| `i8`, `i16`, `i32`, `i64` | `BareCodec.i8`, `BareCodec.i16`, `BareCodec.i32`, `BareCodec.i64` | `sbyte`, `int16`, `int32`, `int64` |
| `f32`, `f64` | `BareCodec.f32`, `BareCodec.f64` | `float32`, `float` |
| `bool`, `str` | `BareCodec.bool`, `BareCodec.string` | `bool`, `string` |
| `data`, `data[n]` | `BareCodec.data`, `BareCodec.fixedData n` | `byte array` |
| `optional<T>` | `BareCodec.option inner` | `'T option` |
| `list<T>`, `list<T>[n]` | `BareCodec.array inner`, `BareCodec.list inner`, `BareCodec.fixedArray n inner` | `'T array`, `'T list`, `'T array` |
| `map<K><V>` | `BareCodec.map key value` | `Map<'K, 'V>` |
| `enum` | `BareCodec.enum name values` | `'T when 'T : equality` |
| `union` | `BareCodec.union cases` (`BareCodec.case`, `BareCodec.caseVoid`) | `'U` |
| `struct` | `bareStruct { ... }` (`BareCodec.field`) | `'R` |

### Structs, unions, and conversions

- **`bareStruct`:** Evaluates once with default field values when constructed to discover field order and schema metadata, and once per decoded record. Bind every field unconditionally with `let!` and finish with `return`; apply validation or mapping per field via `BareCodec.convert` or `BareCodec.refine`.
- **Tagged unions:** Build cases with `BareCodec.case tag payloadCodec inject project` (where `project: 'U -> 'T voption`) or `BareCodec.caseVoid tag typeName value isCase`, then combine them with `BareCodec.union`. Because BARE distinguishes union members by schema type, wrap cases that share the same underlying payload representation in distinct `BareCodec.named` aliases.
- **Mapping and refinement:**
  - `BareCodec.convert (ofWire: 'A -> 'B) (toWire: 'B -> 'A) codec` maps isomorphically over an existing wire representation.
  - `BareCodec.refine (ofWire: 'A -> 'B voption) (toWire: 'B -> 'A) codec` rejects invalid wire values (`ValueNone`) as `BareError.Malformed`.
- **Interoperating with raw BAREWire:** `BareCodec.create schema size write read` wraps an exact-sized BAREWire writer/reader pair; `BareCodec.ofBareWire schema capacity write read` adapts a fixed-capacity pair.

## Schema emission and validation

Every `BareCodec<'T>` carries its complete BARE schema graph:

```fsharp
let schemaDef = BareCodec.schema "Person" person
let text: string = BareCodec.schemaText "Person" person
let errors = BareCodec.validate "Person" person
let bounds = BareCodec.wireSize "Person" person
```

- `BareCodec.schemaText root codec` emits standard BARE schema syntax.
- `BareCodec.validate root codec` runs `BAREWire.Schema.Validation.validate`, catching duplicate union case types, invalid BARE map key types (`f32`, `f64`, `data`, `void`), conflicting type declarations, or duplicate field/enum names. An empty array indicates a valid schema.
- `BareCodec.wireSize root codec` computes minimum/maximum wire byte bounds via `BAREWire.Schema.Analysis.wireSize`.

## Envelope frames: `BareFrame`

`BareFrame` encodes and decodes BAREWire 5-byte message envelopes (1-byte `FrameKind` + 4-byte `uint32` correlation ID + BARE payload):

- `BareFrame.ask correlation codec value` — builds an `Ask` frame.
- `BareFrame.reply correlation codec value` — builds a `Reply` frame echoing `correlation`.
- `BareFrame.tell codec value` — builds a one-way `Tell` frame (`correlation = 0u`).
- `BareFrame.event codec value` — builds a server/peer `Event` frame (`correlation = 0u`).
- `BareFrame.encode frame` / `BareFrame.decode bytes` — converts a `Frame` to/from a self-framed message `byte array` (such as a WebSocket binary message or HTTP body).
- `BareFrame.payload codec frame` — decodes `frame.Payload` with `codec`.
- `BareFrame.kindName kind` — formats `Tell`, `Ask`, `Reply`, `Event`, or `Control`.

## Request/reply and duplex sessions

### Request/response transports (`BareClient`)

For transports that naturally pair one request with one response (such as HTTP `POST`), construct a `BareClient` with `BareClient.OverBytes`:

```fsharp
let bare = BareClient.OverBytes (fun bytes cancellation -> httpPost bytes cancellation)

let fetchPerson id cancellation =
    bare.Ask (BareCodec.uint32, person, id, cancellation)
```

`BareClient` assigns monotonically increasing `uint32` correlation IDs (skipping `0u` on wraparound), verifies that the response is a `Reply` with the matching correlation ID, and decodes the payload. `bare.Asker(requestCodec, replyCodec)` (plus an overload accepting `toRequest` and `ofReply` projections for single-union protocols) curries `Ask` into `'Key -> CancellationToken -> Task<'T>`.

### Multiplexed message transports (`BareDuplex`)

For persistent bidirectional transports such as WebSockets, `BareDuplex` correlates concurrent in-flight asks against incoming `Reply` frames and routes non-reply frames to subscribers:

```fsharp
let duplex = BareDuplex (fun bytes cancellation -> webSocketSend bytes cancellation)
let bare = BareClient (fun frame cancellation -> duplex.Exchange (frame, cancellation))

// Feed incoming binary messages from the transport receive loop:
let onMessage (bytes: byte array) =
    duplex.Receive bytes |> ignore

// Subscribe to typed Event frames:
use events =
    duplex.OnEvent (person, function
        | Ok updated -> printfn "Updated %s" updated.Name
        | Error err -> eprintfn "Decode failed: %s" err.Message)

// Fire-and-forget Tell frames:
do! duplex.Tell (person, ada, cancellationToken)

// Fault pending and future asks when the transport closes:
duplex.Close (System.IO.IOException "Socket closed")
```

To integrate `BareClient` and `BareDuplex` directly with reactive queries, mutations, and graph-thread invalidation, see [Ranvier.Query bindings](queries.md).

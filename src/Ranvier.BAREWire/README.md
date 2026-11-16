# Ranvier.BAREWire

[BAREWire](https://forge.spkez.dev/FidelityFramework/BAREWire) for Ranvier. The package holds BAREWire itself,
compiled for .NET 10, .NET Standard 2.1 and Fable, and a typed layer over it: codecs that carry their BARE schema,
and asks, replies and events over BAREWire's envelope frames. There is no JSON in it.

BAREWire's own modules keep their names (`BAREWire.Encoding`, `BAREWire.Framing`, `BAREWire.Schema`,
`BAREWire.Memory`, `BAREWire.Hardware`, `BAREWire.Platform`), so a writer and reader pair written against BAREWire, by
hand or generated from a schema, compiles against this package unchanged. Do not reference a second build of
BAREWire beside it: both would define the same types.

## Codecs

A `BareCodec<'T>` is BAREWire's writer and reader pair for a type, the exact size of a value, and the schema type the
pair implements.

```fsharp
open Ranvier.BAREWire

type Colour = Red | Green | Blue
type Person = { Name: string; Age: int; Colour: Colour; Nick: string option }

let colour = BareCodec.enum "Colour" [ "RED", 0UL, Red; "GREEN", 1UL, Green; "BLUE", 2UL, Blue ]

let person =
    bareStruct {
        let! name = BareCodec.field "name" _.Name BareCodec.string
        let! age = BareCodec.field "age" _.Age BareCodec.int32
        let! colour = BareCodec.field "colour" _.Colour colour
        let! nick = BareCodec.field "nick" _.Nick (BareCodec.option BareCodec.string)
        return { Name = name; Age = age; Colour = colour; Nick = nick }
    }
    |> BareCodec.named "Person"

let bytes = person.Encode { Name = "Ada"; Age = 36; Colour = Blue; Nick = None }
let back = person.Decode bytes // Ok ..., or Error (BareError.Malformed | Trailing | Shape)
```

`Encode` returns an array of exactly the value's size and raises `ArgumentException` for a value the layout cannot
hold: a null string or array, bytes or a list of another length than a fixed layout's, an enum or union value the
layout does not declare. `Decode` requires every byte to be consumed and returns every failure as an `Error`.

| BARE type | Codec | F# type |
|---|---|---|
| `uint`, `int` | `BareCodec.uint`, `BareCodec.int` | `uint64`, `int64` |
| `uint`, `int`, range-checked | `BareCodec.uint32`, `BareCodec.int32` | `uint32`, `int` |
| `u8` … `u64`, `i8` … `i64` | `BareCodec.u8` … `BareCodec.i64` | `byte` … `int64` |
| `f32`, `f64` | `BareCodec.f32`, `BareCodec.f64` | `float32`, `float` |
| `bool`, `str` | `BareCodec.bool`, `BareCodec.string` | `bool`, `string` |
| `data`, `data[n]` | `BareCodec.data`, `BareCodec.fixedData n` | `byte array` |
| `optional<T>` | `BareCodec.option` | `'T option` |
| `list<T>`, `list<T>[n]` | `BareCodec.array`, `BareCodec.list`, `BareCodec.fixedArray n` | `'T array`, `'T list` |
| `map<K><V>` | `BareCodec.map` | `Map<'K, 'V>` |
| `enum` | `BareCodec.enum` | any type with equality |
| `union` | `BareCodec.union` over `BareCodec.case` and `BareCodec.caseVoid` | any type |
| `struct` | `bareStruct { ... }` | any type |

`BareCodec.convert` and `BareCodec.refine` hold a type on the wire as another. `BareCodec.create` and
`BareCodec.ofBareWire` make a codec from a BAREWire writer and reader pair.

A `bareStruct` expression runs once with a default value for every field when the codec is built, to learn the
fields and their order, and once per decoded value. It holds only `let!` bindings of fields and a final `return`.

BARE tells the cases of a union apart by type, so two cases of one union carry different types: name cases of the
same shape with `BareCodec.named`.

## Schema

Every codec knows its BARE schema, and BAREWire emits, validates and sizes it:

```fsharp
BareCodec.schemaText "Person" person
// type Colour enum { RED = 0 GREEN = 1 BLUE = 2 }
// type Person struct {
//   name: str
//   age: int
//   colour: Colour
//   nick: optional<str>
// }

BareCodec.validate "Person" person // BAREWire's findings; empty when well-formed
BareCodec.wireSize "Person" person // BAREWire's size bounds
```

## Frames, asks and events

`BareFrame` builds and reads BAREWire's envelope: a kind byte, a `u32` correlation id and a BARE payload.
`BareClient` sends a value as an Ask and completes with the value of the Reply that echoes its correlation id.

```fsharp
// A transport that pairs a request with its response, as an HTTP POST of the frame's bytes.
let bare = BareClient.OverBytes (fun bytes cancellation -> post bytes cancellation)
let ada = bare.Ask (BareCodec.uint32, person, 42u, cancellation) // Task<Person>
```

Over a message transport, as a WebSocket, a `BareDuplex` correlates the frames: give it each message that arrives,
and it completes the ask a Reply answers and hands every other frame to its subscribers.

```fsharp
let duplex = BareDuplex (fun bytes cancellation -> socket.Send (bytes, cancellation))
let bare = BareClient (fun frame cancellation -> duplex.Exchange (frame, cancellation))
// in the receive loop: duplex.Receive message |> ignore
use subscription = duplex.OnEvent (person, fun received -> ...)
// when the socket closes: duplex.Close reason
```

A failed ask faults its task: `BareWireException` for an answer that is not the awaited Reply or whose payload does
not decode, and the transport's own failure otherwise.

[Ranvier.BAREWire.Query](https://www.nuget.org/packages/Ranvier.BAREWire.Query) binds asks to Ranvier.Query
families and mutations.

## Targets

.NET 10, .NET Standard 2.1 and Fable. The .NET 10 build is trimmable and NativeAOT-compatible. BAREWire decodes
ill-formed UTF-8 as U+FFFD and writes an unpaired surrogate as U+FFFD; neither is an error.

Under NativeAOT, a codec's value type nested more than about five generic types deep, such as a list of maps of
options of lists, can exceed the compiler's limit on generic instantiation. Name the inner layers as records with
`bareStruct`, which nests no types.

## Licence

Ranvier's code in this package is under the MIT License. BAREWire is copyright SpeakEZ Technologies, Inc. and under
the Apache License 2.0, with a commercial licence available from SpeakEZ Technologies; its files are included
unmodified. `NOTICE.md` and the `licenses` folder of the package hold both licence texts, BAREWire's patent notice
and the commit the sources were taken from.

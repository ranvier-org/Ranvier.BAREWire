/// <summary>
/// One set of checks compiled three ways: by the NativeAOT smoke program, by Fable, and on .NET. It uses no
/// reflection and no formatting, so it builds under <c>--reflectionfree</c>.
/// </summary>
module Ranvier.BAREWire.Smoke.Checks

open System
open System.Threading
open System.Threading.Tasks
open Ranvier
open Ranvier.Query
open Ranvier.BAREWire
open Ranvier.BAREWire.Query

type Colour =
    | Red
    | Green
    | Blue

type Person =
    {
        Name: string
        Age: int
        Colour: Colour
        Nick: string option
        Scores: Map<string, int64>
        Ratio: float
    }

type Shape =
    | Circle of radius: float
    | Empty

let private colour: BareCodec<Colour> =
    BareCodec.enum "Colour" [ "RED", 0UL, Red; "GREEN", 1UL, Green; "BLUE", 2UL, Blue ]

let private person: BareCodec<Person> =
    bareStruct {
        let! name = BareCodec.field "name" _.Name BareCodec.string
        let! age = BareCodec.field "age" _.Age BareCodec.int32
        let! colour = BareCodec.field "colour" _.Colour colour
        let! nick = BareCodec.field "nick" _.Nick (BareCodec.option BareCodec.string)

        let! scores =
            BareCodec.field "scores" _.Scores (BareCodec.map BareCodec.string BareCodec.int)

        let! ratio = BareCodec.field "ratio" _.Ratio BareCodec.f64

        return
            {
                Name = name
                Age = age
                Colour = colour
                Nick = nick
                Scores = scores
                Ratio = ratio
            }
    }
    |> BareCodec.named "Person"

let private shape: BareCodec<Shape> =
    BareCodec.union
        [
            BareCodec.case 0 (BareCodec.named "Circle" BareCodec.f64) Circle (function
                | Circle radius -> ValueSome radius
                | _ -> ValueNone)
            BareCodec.caseVoid 1 "Empty" Empty (function
                | Empty -> true
                | _ -> false)
        ]

let private ada =
    {
        Name = "Ada \U0001F600"
        Age = -36
        Colour = Blue
        Nick = Some "héllo"
        Scores = Map.ofList [ "least", Int64.MinValue; "most", Int64.MaxValue ]
        Ratio = 0.1
    }

let private expectedSchema =
    String.concat
        "\n"
        [
            "type Colour enum { RED = 0 GREEN = 1 BLUE = 2 }"
            "type Person struct {"
            "  name: str"
            "  age: int"
            "  colour: Colour"
            "  nick: optional<str>"
            "  scores: map<str><int>"
            "  ratio: f64"
            "}"
        ]

let private completed (value: 'T) : Task<'T> =
    let source = TaskCompletionSource<'T>()
    source.SetResult value
    source.Task

/// <summary>A peer that answers an Ask carrying a <c>uint</c> with the person of that age.</summary>
let private peer (bytes: byte array) (_: CancellationToken) : Task<byte array> =
    match BareFrame.decode bytes with
    | Error error -> raise (BareWireException error)
    | Ok frame ->
        match BareFrame.payload BareCodec.uint32 frame with
        | Error error -> raise (BareWireException error)
        | Ok age -> completed (BareFrame.encode (BareFrame.reply frame.Correlation person { ada with Age = int age }))

let private codecChecks () : (string * bool) list =
    [
        "uint 300 is AC 02", BareCodec.uint.Encode 300UL = [| 0xACuy; 0x02uy |]
        "int -1 is 01", BareCodec.int.Encode -1L = [| 0x01uy |]
        "f64 1.0 is its IEEE-754 bytes", BareCodec.f64.Encode 1.0 = [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0xF0uy; 0x3Fuy |]
        "f32 1.0 is its IEEE-754 bytes", BareCodec.f32.Encode 1.0f = [| 0uy; 0uy; 0x80uy; 0x3Fuy |]
        "f64 round trips", BareCodec.f64.Decode (BareCodec.f64.Encode 0.1) = Ok 0.1
        "f32 round trips", BareCodec.f32.Decode (BareCodec.f32.Encode 1.5f) = Ok 1.5f
        "u64 is little-endian", BareCodec.u64.Encode 0x0123456789ABCDEFUL = [| 0xEFuy; 0xCDuy; 0xABuy; 0x89uy; 0x67uy; 0x45uy; 0x23uy; 0x01uy |]
        "uint round trips at 64 bits", BareCodec.uint.Decode (BareCodec.uint.Encode UInt64.MaxValue) = Ok UInt64.MaxValue
        "int round trips at its least value", BareCodec.int.Decode (BareCodec.int.Encode Int64.MinValue) = Ok Int64.MinValue
        "a string outside the BMP round trips", BareCodec.string.Decode (BareCodec.string.Encode ada.Name) = Ok ada.Name
        "a struct round trips", person.Decode (person.Encode ada) = Ok ada
        "a union round trips",
        shape.Decode (shape.Encode (Circle 2.5)) = Ok (Circle 2.5)
        && shape.Encode Empty = [| 1uy |]
        "the schema is BAREWire's text", BareCodec.schemaText "Person" person = expectedSchema
        "the schema validates", (BareCodec.validate "Person" person).Length = 0
        "truncated bytes are Malformed",
        (match person.Decode (Array.sub (person.Encode ada) 0 5) with
         | Error (BareError.Malformed _) -> true
         | _ -> false)
        "a frame is kind, correlation, payload",
        BareFrame.encode (BareFrame.ask 0x01020304u BareCodec.string "hi") = [| 1uy; 4uy; 3uy; 2uy; 1uy; 2uy; 0x68uy; 0x69uy |]
    ]

/// <summary>Each check's name and whether it held.</summary>
let run () : Task<(string * bool) list> =
    task {
        let bare = BareClient.OverBytes peer
        let! asked = bare.Ask (BareCodec.uint32, person, 41u, CancellationToken.None)

        use graph =
            new Graph (
                { GraphOptions.Default with
                    ThreadAffinity = ThreadAffinity.Unchecked
                    Dispatcher = Some (ImmediateDispatcher ())
                }
            )

        use _active = graph.Activate ()
        use client = new QueryClient (graph)
        let people = client.DefineBare (bare, BareCodec.uint32, person)
        use query = people.Acquire 42u
        let! loaded = query.EnsureAsync ()

        return
            codecChecks ()
            @ [
                "an ask completes with its reply", asked = { ada with Age = 41 }
                "a query loads through an ask",
                loaded = { ada with Age = 42 }
                && query.State.Data = Some loaded
            ]
    }

module Ranvier.BAREWire.Tests.CodecTests

open System
open Expecto
open FsCheck
open BAREWire.Encoding
open BAREWire.Framing
open BAREWire.Schema
open Ranvier.BAREWire
open Ranvier.BAREWire.Tests.Model
open Ranvier.BAREWire.Tests.Support

let private bytes (values: int list) : byte array =
    values |> List.map byte |> List.toArray

let private isMalformed (result: Result<'T, BareError>) =
    match result with
    | Error (BareError.Malformed _) -> true
    | _ -> false

/// <summary>A string as BAREWire reads it back: an unpaired surrogate becomes U+FFFD.</summary>
let private normalise (text: string) : string =
    Text.ofUtf8 (Text.toUtf8 text)

[<Tests>]
let vectors =
    testList
        "golden vectors"
        [
            testCase "the primitives write BARE's bytes"
            <| fun () ->
                Expect.equal (BareCodec.uint.Encode 300UL) (bytes [ 0xAC; 0x02 ]) "uint 300"
                Expect.equal (BareCodec.int.Encode -1L) (bytes [ 0x01 ]) "int -1"
                Expect.equal (BareCodec.int.Encode 64L) (bytes [ 0x80; 0x01 ]) "int 64"
                Expect.equal (BareCodec.int32.Encode 36) (bytes [ 0x48 ]) "int 36 as an F# int"
                Expect.equal (BareCodec.uint32.Encode 128u) (bytes [ 0x80; 0x01 ]) "uint 128 as an F# uint32"
                Expect.equal (BareCodec.u16.Encode 0x1234us) (bytes [ 0x34; 0x12 ]) "u16"
                Expect.equal (BareCodec.i32.Encode -3) (bytes [ 0xFD; 0xFF; 0xFF; 0xFF ]) "i32 -3"
                Expect.equal (BareCodec.f64.Encode 1.0) (bytes [ 0; 0; 0; 0; 0; 0; 0xF0; 0x3F ]) "f64 1.0"
                Expect.equal (BareCodec.bool.Encode true) (bytes [ 0x01 ]) "bool"
                Expect.equal (BareCodec.string.Encode "hello") (bytes [ 0x05; 0x68; 0x65; 0x6C; 0x6C; 0x6F ]) "str"
                Expect.equal (BareCodec.data.Encode (bytes [ 1; 2; 3 ])) (bytes [ 0x03; 1; 2; 3 ]) "data"
                Expect.equal ((BareCodec.fixedData 2).Encode(bytes [ 1; 2 ])) (bytes [ 1; 2 ]) "data[2]"

            testCase "the aggregates write BARE's bytes"
            <| fun () ->
                Expect.equal ((BareCodec.option BareCodec.u8).Encode None) (bytes [ 0x00 ]) "optional absent"
                Expect.equal ((BareCodec.option BareCodec.u8).Encode(Some 42uy)) (bytes [ 0x01; 0x2A ]) "optional present"
                Expect.equal ((BareCodec.array BareCodec.uint).Encode [| 1UL; 128UL |]) (bytes [ 0x02; 0x01; 0x80; 0x01 ]) "list"
                Expect.equal ((BareCodec.list BareCodec.u8).Encode [ 7uy; 8uy ]) (bytes [ 0x02; 7; 8 ]) "list from an F# list"
                Expect.equal ((BareCodec.fixedArray 2 BareCodec.u8).Encode [| 7uy; 8uy |]) (bytes [ 7; 8 ]) "list[2]"

                Expect.equal
                    ((BareCodec.map BareCodec.string BareCodec.int).Encode(Map.ofList [ "b", 1L; "a", -1L ]))
                    (bytes [ 0x02; 0x01; 0x61; 0x01; 0x01; 0x62; 0x02 ])
                    "map, keys in order"

                Expect.equal (colour.Encode Blue) (bytes [ 0x02 ]) "enum"
                Expect.equal (shape.Encode Empty) (bytes [ 0x04 ]) "union, void case"
                Expect.equal (shape.Encode (Circle 1.0)) (bytes [ 0x00; 0; 0; 0; 0; 0; 0; 0xF0; 0x3F ]) "union, tag then value"

            testCase "a struct is its fields in order"
            <| fun () ->
                let expected =
                    bytes
                        [
                            0x03
                            0x41
                            0x64
                            0x61 // name "Ada"
                            0x48 // age 36
                            0x02 // colour BLUE
                            0x01
                            0x03
                            0x41
                            0x41
                            0x4C // nick "AAL"
                            0x02 // two scores
                            0x08
                            0x61
                            0x6E
                            0x61
                            0x6C
                            0x79
                            0x73
                            0x69
                            0x73
                            0x14 // "analysis" 10
                            0x06
                            0x70
                            0x6F
                            0x65
                            0x74
                            0x72
                            0x79
                            0x03 // "poetry" -2
                            0x02 // two tags
                            0x08
                            0x63
                            0x6F
                            0x75
                            0x6E
                            0x74
                            0x65
                            0x73
                            0x73
                            0x07
                            0x61
                            0x6E
                            0x61
                            0x6C
                            0x79
                            0x73
                            0x74
                        ]

                Expect.equal (person.Encode ada) expected "encoded"
                Expect.equal (person.Decode expected) (Ok ada) "decoded"
        ]

[<Tests>]
let interop =
    testList
        "BAREWire interop"
        [
            testCase "a Ranvier struct codec reads and writes BAREWire's Hello"
            <| fun () ->
                let hello: BareCodec<Hello> =
                    bareStruct {
                        let! epoch = BareCodec.field "epoch" _.Epoch BareCodec.int32
                        let! build = BareCodec.field "build" _.Build BareCodec.string
                        return { Epoch = epoch; Build = build }
                    }

                let frame = Envelope.hello 7 "build-1"
                Expect.equal (hello.Decode frame.Payload) (Ok { Epoch = 7; Build = "build-1" }) "reads BAREWire's bytes"

                let written = hello.Encode { Epoch = -12; Build = "héllo" }
                let read, consumed = Codec.decode Envelope.readHello written
                Expect.equal consumed written.Length "BAREWire consumed every byte"
                Expect.equal read { Epoch = -12; Build = "héllo" } "BAREWire reads Ranvier's bytes"

            testCase "a BAREWire writer and reader pair becomes a codec"
            <| fun () ->
                let schema =
                    Bare.struct' [| Bare.field "epoch" Bare.int; Bare.field "build" Bare.string |]

                let hello = BareCodec.ofBareWire schema 64 Envelope.writeHello Envelope.readHello
                let value = { Epoch = 3; Build = "b" }
                Expect.equal (hello.Decode (hello.Encode value)) (Ok value) "round trip"
                Expect.equal hello.TypeText "struct { epoch: int build: str }" "schema text"

                Expect.throwsT<ArgumentException>
                    (fun () ->
                        hello.Encode { Epoch = 3; Build = String ('x', 100) }
                        |> ignore)
                    "a value past the declared capacity has no encoding"

            testCase "a codec's pair is a BAREWire codec"
            <| fun () ->
                let encoded, length = Codec.encode 256 person.Write ada
                Expect.equal length encoded.Length "length"
                Expect.equal encoded (person.Encode ada) "the same bytes through BAREWire's encode"
                let decoded, consumed = Codec.decode person.Read encoded
                Expect.equal consumed length "consumed"
                Expect.equal decoded ada "the same value through BAREWire's decode"
        ]

[<Tests>]
let failures =
    testList
        "failures"
        [
            testCase "bytes that are not the layout are Malformed"
            <| fun () ->
                Expect.isTrue (isMalformed (BareCodec.bool.Decode (bytes [ 2 ]))) "a bool byte of 2"
                Expect.isTrue (isMalformed (BareCodec.u16.Decode (bytes [ 1 ]))) "a short u16"
                Expect.isTrue (isMalformed (BareCodec.string.Decode (bytes [ 5; 0x68 ]))) "a string shorter than its length"
                Expect.isTrue (isMalformed (BareCodec.string.Decode null)) "null bytes"
                Expect.isTrue (isMalformed (colour.Decode (bytes [ 3 ]))) "an undeclared enum value"
                Expect.isTrue (isMalformed (shape.Decode (bytes [ 2 ]))) "an undeclared union tag"
                Expect.isTrue (isMalformed (BareCodec.int32.Decode (BareCodec.int.Encode 2147483648L))) "an int past int32"
                Expect.isTrue (isMalformed (BareCodec.uint32.Decode (BareCodec.uint.Encode 4294967296UL))) "a uint past uint32"
                Expect.isTrue (isMalformed ((BareCodec.option BareCodec.u8).Decode(bytes [ 2; 0 ]))) "an optional tag of 2"

                Expect.isTrue
                    (isMalformed ((BareCodec.array BareCodec.u8).Decode(bytes [ 0xFF; 0xFF; 0xFF; 0xFF; 0x07 ])))
                    "a list count past the bytes that remain"

                Expect.isTrue
                    (isMalformed ((BareCodec.map BareCodec.string BareCodec.int).Decode(bytes [ 0x02; 0x01; 0x61; 0x02; 0x01; 0x61; 0x04 ])))
                    "a repeated map key"

                Expect.isTrue (isMalformed (person.Decode (Array.sub (person.Encode ada) 0 20))) "a truncated struct"

            testCase "bytes after the value are Trailing"
            <| fun () ->
                match BareCodec.u8.Decode (bytes [ 1; 2 ]) with
                | Error (BareError.Trailing message) -> Expect.stringContains message "offset 1 of 2" "names where the value ended"
                | other -> failtestf "expected Trailing, got %A" other

            testCase "a raising conversion is Shape, and never runs on a faulted read"
            <| fun () ->
                let mutable calls = 0

                let positive =
                    BareCodec.int32
                    |> BareCodec.convert
                        (fun value ->
                            calls <- calls + 1
                            if value < 0 then failwith "negative" else value)
                        id

                Expect.equal (positive.Decode (bytes [ 0x01 ])) (Error (BareError.Shape "negative")) "the conversion's message"
                Expect.isTrue (isMalformed (positive.Decode (bytes [ 0x80 ]))) "a truncated varint"
                Expect.equal calls 1 "the conversion ran once, on the decoded value only"

            testCase "refine faults on a wire value the type does not hold"
            <| fun () ->
                let even =
                    BareCodec.uint32
                    |> BareCodec.refine (fun value -> if value % 2u = 0u then ValueSome value else ValueNone) id

                Expect.equal (even.Decode (bytes [ 4 ])) (Ok 4u) "an even value"
                Expect.isTrue (isMalformed (even.Decode (bytes [ 3 ]))) "an odd value"

            testCase "a value the layout cannot hold raises on Encode"
            <| fun () ->
                Expect.throwsT<ArgumentException> (fun () -> BareCodec.string.Encode null |> ignore) "a null string"
                Expect.throwsT<ArgumentException> (fun () -> BareCodec.data.Encode null |> ignore) "null data"

                Expect.throwsT<ArgumentException>
                    (fun () ->
                        (BareCodec.fixedData 2).Encode(bytes [ 1 ])
                        |> ignore)
                    "data of another length"

                Expect.throwsT<ArgumentException>
                    (fun () ->
                        (BareCodec.fixedArray 2 BareCodec.u8).Encode [| 1uy |]
                        |> ignore)
                    "a list of another length"

                let primary = BareCodec.enum "Primary" [ "RED", 0UL, Red ]
                Expect.throwsT<ArgumentException> (fun () -> primary.Encode Blue |> ignore) "an undeclared enum value"

                let round =
                    BareCodec.union
                        [
                            BareCodec.case 0 BareCodec.f64 Circle (function
                                | Circle radius -> ValueSome radius
                                | _ -> ValueNone)
                        ]

                Expect.throwsT<ArgumentException> (fun () -> round.Encode Empty |> ignore) "a union value no case claims"

                Expect.throwsT<ArgumentException>
                    (fun () ->
                        person.Encode { ada with Nick = Some null }
                        |> ignore)
                    "a null string inside a struct"

            testCase "a layout BARE cannot express is refused when it is built"
            <| fun () ->
                Expect.throwsT<ArgumentException> (fun () -> BareCodec.fixedData 0 |> ignore) "data[0]"
                Expect.throwsT<ArgumentException> (fun () -> BareCodec.fixedArray 0 BareCodec.u8 |> ignore) "list[0]"

                Expect.throwsT<ArgumentException>
                    (fun () ->
                        BareCodec.union ([]: BareCase<Shape> list)
                        |> ignore)
                    "an empty union"

                Expect.throwsT<ArgumentException>
                    (fun () ->
                        BareCodec.enum "Colour" ([]: (string * uint64 * Colour) list)
                        |> ignore)
                    "an empty enum"

                let empty tag =
                    BareCodec.caseVoid tag "Empty" Empty (function
                        | Empty -> true
                        | _ -> false)

                Expect.throwsT<ArgumentException> (fun () -> BareCodec.union [ empty 1; empty 1 ] |> ignore) "a repeated tag"
                Expect.throwsT<ArgumentException> (fun () -> BareCodec.union [ empty -1 ] |> ignore) "a negative tag"
        ]

[<Tests>]
let schemas =
    testList
        "schema"
        [
            testCase "a codec emits its BARE schema through BAREWire"
            <| fun () ->
                let expected =
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
                            "  tags: list<str>"
                            "}"
                        ]

                Expect.equal (BareCodec.schemaText "Person" person) expected "schema text"
                Expect.isEmpty (BareCodec.validate "Person" person) "well-formed"

            testCase "a union declares its named cases before itself"
            <| fun () ->
                let expected =
                    String.concat
                        "\n"
                        [
                            "type Circle f64"
                            "type Size struct {"
                            "  width: f64"
                            "  height: f64"
                            "}"
                            "type Empty void"
                            "type Shape union { Circle | Size | Empty = 4 }"
                        ]

                Expect.equal (BareCodec.schemaText "Shape" shape) expected "schema text"
                Expect.isEmpty (BareCodec.validate "Shape" shape) "well-formed"

            testCase "BAREWire's validation reports what the combinators accept"
            <| fun () ->
                let twoFloats =
                    BareCodec.union
                        [
                            BareCodec.case 0 BareCodec.f64 Circle (fun _ -> ValueNone)
                            BareCodec.case 1 BareCodec.f64 Circle (fun _ -> ValueNone)
                        ]

                let kinds codec =
                    BareCodec.validate "Root" codec
                    |> Array.map _.Kind
                    |> Array.toList

                Expect.equal (kinds twoFloats) [ ValidationErrorKind.DuplicateCaseType ] "two cases of one type"

                Expect.equal (kinds (BareCodec.map BareCodec.f64 BareCodec.u8)) [ ValidationErrorKind.InvalidMapKeyType ] "a map keyed by f64"

                let pair =
                    bareStruct {
                        let! left = BareCodec.field "left" fst (BareCodec.named "Id" BareCodec.u8)
                        let! right = BareCodec.field "right" snd (BareCodec.named "Id" BareCodec.u16)
                        return (left, right)
                    }

                Expect.contains (kinds pair) ValidationErrorKind.DuplicateTypeName "one name declared as two types"

            testCase "BAREWire sizes a codec's type"
            <| fun () ->
                match BareCodec.wireSize "Size" size with
                | Ok bounds ->
                    Expect.isTrue bounds.IsFixed "two f64 fields are fixed width"
                    Expect.equal bounds.Min 16 "sixteen bytes"
                | Error findings -> failtestf "expected a size, got %A" findings

                match BareCodec.wireSize "Person" person with
                | Ok bounds -> Expect.isFalse bounds.IsBounded "a struct holding strings has no upper bound"
                | Error findings -> failtestf "expected a size, got %A" findings
        ]

[<Tests>]
let properties =
    testList
        "properties"
        [
            testProperty "a string encodes as BAREWire writes it and decodes as BAREWire reads it"
            <| fun (text: NonNull<string>) ->
                let encoded = BareCodec.string.Encode text.Get

                let viaBareWire, length =
                    Codec.encode (encoded.Length + 16) Encoder.writeString text.Get

                encoded = viaBareWire
                && length = encoded.Length
                && BareCodec.string.Decode encoded = Ok (normalise text.Get)

            testProperty "varints round trip at their exact size"
            <| fun (unsigned: uint64) (signed: int64) (small: int) ->
                BareCodec.uint.Decode (BareCodec.uint.Encode unsigned) = Ok unsigned
                && BareCodec.int.Decode (BareCodec.int.Encode signed) = Ok signed
                && BareCodec.int32.Decode (BareCodec.int32.Encode small) = Ok small

            testProperty "floats keep their bits"
            <| fun (double: float) (single: float32) ->
                let doubleBack = ok (BareCodec.f64.Decode (BareCodec.f64.Encode double))
                let singleBack = ok (BareCodec.f32.Decode (BareCodec.f32.Encode single))

                BitConverter.DoubleToInt64Bits doubleBack = BitConverter.DoubleToInt64Bits double
                && BitConverter.SingleToInt32Bits singleBack = BitConverter.SingleToInt32Bits single

            testProperty "a struct round trips, and no proper prefix of its bytes decodes"
            <| fun
                   (name: NonNull<string>)
                   (age: int)
                   (nick: NonNull<string> option)
                   (scores: (NonNull<string> * int64) list)
                   (tags: NonNull<string> list) ->
                let value =
                    {
                        Name = normalise name.Get
                        Age = age
                        Colour = (if age % 2 = 0 then Red else Green)
                        Nick =
                            nick
                            |> Option.map (fun text -> normalise text.Get)
                        Scores =
                            scores
                            |> List.map (fun (key, score) -> normalise key.Get, score)
                            |> Map.ofList
                        Tags = tags |> List.map (fun tag -> normalise tag.Get)
                    }

                let encoded = person.Encode value
                let cut = encoded.Length / 2

                person.Decode encoded = Ok value
                && (match person.Decode (Array.sub encoded 0 cut) with
                    | Error _ -> true
                    | Ok _ -> false)

            testProperty "decoding arbitrary bytes returns a result and never raises"
            <| fun (noise: byte array) ->
                let noise = if isNull noise then Array.empty else noise

                match person.Decode noise, shape.Decode noise with
                | (Ok _ | Error _), (Ok _ | Error _) -> true
        ]

type Wide =
    {
        A: int
        B: string
        C: int
        D: string
        E: int
        F: string
        G: int
        H: string
        I: int
        J: string
        K: int
        L: string
    }

let private wide: BareCodec<Wide> =
    bareStruct {
        let! a = BareCodec.field "a" _.A BareCodec.int32
        let! b = BareCodec.field "b" _.B BareCodec.string
        let! c = BareCodec.field "c" _.C BareCodec.int32
        let! d = BareCodec.field "d" _.D BareCodec.string
        let! e = BareCodec.field "e" _.E BareCodec.int32
        let! f = BareCodec.field "f" _.F BareCodec.string
        let! g = BareCodec.field "g" _.G BareCodec.int32
        let! h = BareCodec.field "h" _.H BareCodec.string
        let! i = BareCodec.field "i" _.I BareCodec.int32
        let! j = BareCodec.field "j" _.J BareCodec.string
        let! k = BareCodec.field "k" _.K BareCodec.int32
        let! l = BareCodec.field "l" _.L BareCodec.string

        return
            {
                A = a
                B = b
                C = c
                D = d
                E = e
                F = f
                G = g
                H = h
                I = i
                J = j
                K = k
                L = l
            }
    }

[<Tests>]
let structs =
    testList
        "bareStruct"
        [
            testCase "a struct of twelve fields keeps their order"
            <| fun () ->
                let value =
                    {
                        A = 1
                        B = "b"
                        C = -3
                        D = "d"
                        E = 5
                        F = "f"
                        G = -7
                        H = "h"
                        I = 9
                        J = "j"
                        K = -11
                        L = "l"
                    }

                Expect.equal (wide.Decode (wide.Encode value)) (Ok value) "round trip"

                Expect.equal
                    wide.TypeText
                    "struct { a: int b: str c: int d: str e: int f: str g: int h: str i: int j: str k: int l: str }"
                    "the fields in binding order"

            testCase "a codec is built once and decodes many values"
            <| fun () ->
                let first = person.Decode (person.Encode ada)
                let second = person.Decode (person.Encode { ada with Name = "Grace"; Age = 85 })
                Expect.equal first (Ok ada) "the first value"
                Expect.equal second (Ok { ada with Name = "Grace"; Age = 85 }) "a later value does not see the first one's fields"

            testCase "an expression that raises on default field values is refused when the codec is built"
            <| fun () ->
                let build () =
                    bareStruct {
                        let! name = BareCodec.field "name" fst BareCodec.string
                        let! age = BareCodec.field "age" snd BareCodec.int32
                        return ((name: string).Trim(), age)
                    }

                let error = Expect.throwsC (fun () -> build () |> ignore) id
                Expect.isTrue (error :? InvalidOperationException) "InvalidOperationException"
                Expect.stringContains error.Message "binding the field age" "names the last field bound"
        ]

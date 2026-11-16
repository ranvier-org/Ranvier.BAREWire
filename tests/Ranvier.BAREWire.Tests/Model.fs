module Ranvier.BAREWire.Tests.Model

open Ranvier.BAREWire

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
        Tags: string list
    }

type Size = { Width: float; Height: float }

type Shape =
    | Circle of radius: float
    | Rect of Size
    | Empty

let colour: BareCodec<Colour> =
    BareCodec.enum "Colour" [ "RED", 0UL, Red; "GREEN", 1UL, Green; "BLUE", 2UL, Blue ]

let person: BareCodec<Person> =
    bareStruct {
        let! name = BareCodec.field "name" _.Name BareCodec.string
        let! age = BareCodec.field "age" _.Age BareCodec.int32
        let! colour = BareCodec.field "colour" _.Colour colour
        let! nick = BareCodec.field "nick" _.Nick (BareCodec.option BareCodec.string)

        let! scores =
            BareCodec.field "scores" _.Scores (BareCodec.map BareCodec.string BareCodec.int)

        let! tags = BareCodec.field "tags" _.Tags (BareCodec.list BareCodec.string)

        return
            {
                Name = name
                Age = age
                Colour = colour
                Nick = nick
                Scores = scores
                Tags = tags
            }
    }
    |> BareCodec.named "Person"

let size: BareCodec<Size> =
    bareStruct {
        let! width = BareCodec.field "width" _.Width BareCodec.f64
        let! height = BareCodec.field "height" _.Height BareCodec.f64
        return { Width = width; Height = height }
    }
    |> BareCodec.named "Size"

let shape: BareCodec<Shape> =
    BareCodec.union
        [
            BareCodec.case 0 (BareCodec.named "Circle" BareCodec.f64) Circle (function
                | Circle radius -> ValueSome radius
                | _ -> ValueNone)
            BareCodec.case 1 size Rect (function
                | Rect size -> ValueSome size
                | _ -> ValueNone)
            BareCodec.caseVoid 4 "Empty" Empty (function
                | Empty -> true
                | _ -> false)
        ]

let ada =
    {
        Name = "Ada"
        Age = 36
        Colour = Blue
        Nick = Some "AAL"
        Scores = Map.ofList [ "analysis", 10L; "poetry", -2L ]
        Tags = [ "countess"; "analyst" ]
    }

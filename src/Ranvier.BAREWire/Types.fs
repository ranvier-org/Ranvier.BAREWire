namespace Ranvier.BAREWire

open System
open BAREWire.Encoding
open BAREWire.Schema

/// <summary>Why bytes failed to decode, or why a frame was refused.</summary>
[<RequireQualifiedAccess>]
type BareError =
    /// <summary>
    /// BAREWire's reader faulted: the bytes end early, or hold a value the layout cannot hold (a bool byte other than
    /// 0 or 1, a union tag or enum value the layout does not declare, a repeated map key, a varint past 64 bits).
    /// </summary>
    | Malformed of message: string
    /// <summary>The bytes hold a whole value followed by more bytes.</summary>
    | Trailing of message: string
    /// <summary>A function supplied to the codec raised while building the decoded value.</summary>
    | Shape of message: string
    /// <summary>
    /// The envelope is not the expected frame: a short header, an unknown kind byte, a kind other than the one
    /// awaited, or a correlation id that answers a different ask.
    /// </summary>
    | Frame of message: string

    /// <summary>The text carried by the case.</summary>
    member this.Message =
        match this with
        | Malformed message
        | Trailing message
        | Shape message
        | Frame message -> message

/// <summary>A <see cref="T:Ranvier.BAREWire.BareError"/> raised where a result cannot be returned, as from a task.</summary>
type BareWireException(error: BareError) =
    inherit Exception(error.Message)

    /// <summary>The failure.</summary>
    member _.Error = error

/// <summary>Sizes and sentinels shared by the codecs.</summary>
module internal Wire =

    /// <summary>What a union case answers for a value of another case. Distinct from <c>Cursor.Fault</c>.</summary>
    [<Literal>]
    let NotThisCase = -2

    /// <summary>The sum of two sizes, or <c>Cursor.Fault</c> when either is a fault or the sum leaves <c>int</c>.</summary>
    let add (a: int) (b: int) : int =
        if a < 0 || b < 0 then
            Cursor.Fault
        else
            let sum = a + b
            if sum < 0 then Cursor.Fault else sum

    /// <summary>The bytes ULEB128 uses for a value.</summary>
    let uintSize (value: uint64) : int =
        Analysis.varintSize value

    /// <summary>The zigzag image <c>Encoder.writeInt</c> writes.</summary>
    let zigzag (value: int64) : uint64 =
        (uint64 (value <<< 1)) ^^^ (uint64 (value >>> 63))

    /// <summary>
    /// The length of <c>Text.toUtf8</c>'s output without building it: an unpaired surrogate counts three bytes, as
    /// the U+FFFD that replaces it.
    /// </summary>
    let utf8Length (s: string) : int =
        let mutable n = 0
        let mutable i = 0
        let len = s.Length

        while i < len do
            let c = int s.[i]

            if c < 0x80 then
                n <- n + 1
                i <- i + 1
            elif c < 0x800 then
                n <- n + 2
                i <- i + 1
            elif
                c >= 0xD800
                && c <= 0xDBFF
                && i + 1 < len
                && int s.[i + 1] >= 0xDC00
                && int s.[i + 1] <= 0xDFFF
            then
                n <- n + 4
                i <- i + 2
            else
                n <- n + 3
                i <- i + 1

        n

    /// <summary>The size of a length prefix followed by that many bytes.</summary>
    let prefixed (length: int) : int =
        add (uintSize (uint64 length)) length

    /// <summary>The declarations of two layouts in order, a declaration repeated verbatim kept once.</summary>
    let mergeDeclarations (a: NamedType array) (b: NamedType array) : NamedType array =
        if b.Length = 0 then
            a
        elif a.Length = 0 then
            b
        else
            let merged = ResizeArray<NamedType> a

            for declaration in b do
                let mutable known = false

                for existing in merged do
                    if
                        existing.Name = declaration.Name
                        && existing.Type = declaration.Type
                    then
                        known <- true

                if not known then
                    merged.Add declaration

            merged.ToArray ()

/// <summary>
/// A BARE layout for <c>'T</c>: BAREWire's writer and reader pair, the exact size of a value, and the schema type the
/// pair implements.
/// </summary>
/// <remarks>
/// <c>Write</c> and <c>Read</c> have the shape of <c>BAREWire.Encoding.Encoder</c> and <c>Decoder</c>, so a pair
/// written by hand or generated from a schema becomes a codec through <c>BareCodec.create</c>, and a codec's pair can
/// be passed to BAREWire. Both follow the cursor convention: a fault offset in is a fault offset out, and a faulted
/// read returns <c>Unchecked.defaultof</c>.
/// </remarks>
[<NoEquality; NoComparison>]
type BareCodec<'T> =
    {
        /// <summary>The BARE type of the layout.</summary>
        Schema: SchemaType
        /// <summary>The named types the layout refers to, each after the types it refers to itself.</summary>
        Declarations: NamedType array
        /// <summary>The exact encoded size of a value, or <c>Cursor.Fault</c> for a value the layout cannot hold.</summary>
        Size: 'T -> int
        /// <summary>Writes a value at an offset and returns the next offset, or <c>Cursor.Fault</c>.</summary>
        Write: byte array -> int -> 'T -> int
        /// <summary>Reads a value at an offset, with the next offset or <c>Cursor.Fault</c>.</summary>
        Read: byte array -> int -> 'T * int
    }

    /// <summary>The layout's BARE type in schema text, as <c>struct { name: str born: i64 }</c>.</summary>
    member codec.TypeText: string =
        Emit.typeText
            {
                Types = codec.Declarations
                Root = ""
            }
            codec.Schema

    /// <summary>
    /// The value's BARE encoding, in an array of exactly its size. Raises <c>ArgumentException</c> for a value the
    /// layout cannot hold: a null string or array, bytes or a list of another length than a fixed layout's, an enum
    /// or union value the layout does not declare.
    /// </summary>
    member codec.Encode(value: 'T) : byte array =
        let size = codec.Size value

        if size < 0 then
            invalidArg
                "value"
                ("The value has no BARE encoding as "
                 + codec.TypeText
                 + ".")

        let data: byte array = Array.zeroCreate size
        let next = codec.Write data 0 value

        if next <> size then
            invalidArg
                "value"
                ("The codec for "
                 + codec.TypeText
                 + " sized the value at "
                 + string size
                 + " bytes and its writer ended at offset "
                 + string next
                 + ".")

        data

    /// <summary>
    /// The value the bytes encode, requiring every byte to be consumed; every failure is an <c>Error</c>, and null
    /// bytes are <c>BareError.Malformed</c>.
    /// </summary>
    member codec.Decode(bytes: byte array) : Result<'T, BareError> =
        if isNull bytes then
            Error (BareError.Malformed "The bytes are null.")
        else
            try
                let value, next = codec.Read bytes 0

                if Cursor.isFault next then
                    Error (
                        BareError.Malformed (
                            "The "
                            + string bytes.Length
                            + " bytes are not a BARE encoding of "
                            + codec.TypeText
                            + "."
                        )
                    )
                elif next <> bytes.Length then
                    Error (
                        BareError.Trailing (
                            "A "
                            + codec.TypeText
                            + " ends at offset "
                            + string next
                            + " of "
                            + string bytes.Length
                            + " bytes."
                        )
                    )
                else
                    Ok value
            with ex ->
                Error (BareError.Shape ex.Message)

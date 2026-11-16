namespace Ranvier.BAREWire

open BAREWire.Encoding
open BAREWire.Schema

/// <summary>One case of a BARE union over <c>'U</c>: its wire tag and the layout of the value it carries.</summary>
/// <remarks>Build one with <c>BareCodec.case</c> or <c>BareCodec.caseVoid</c>.</remarks>
[<NoEquality; NoComparison>]
type BareCase<'U> =
    {
        /// <summary>The case's wire tag.</summary>
        Tag: int
        /// <summary>The BARE type the case carries.</summary>
        CaseSchema: SchemaType
        /// <summary>The named types the case refers to.</summary>
        CaseDeclarations: NamedType array
        /// <summary>The size of the tag and value, <c>-2</c> for a value of another case, or <c>Cursor.Fault</c>.</summary>
        CaseSize: 'U -> int
        /// <summary>Writes the tag and value; <c>-2</c> for a value of another case.</summary>
        CaseWrite: byte array -> int -> 'U -> int
        /// <summary>Reads the value that follows the tag.</summary>
        CaseRead: byte array -> int -> 'U * int
    }

/// <summary>A struct field: its schema name, how to read it from the struct value, and its codec.</summary>
/// <remarks>Build one with <c>BareCodec.field</c> and bind it with <c>let!</c> in a <c>bareStruct</c> expression.</remarks>
[<Sealed>]
type BareField<'R, 'F>(name: string, get: 'R -> 'F, codec: BareCodec<'F>) =
    /// <summary>The field's name in the schema.</summary>
    member _.Name = name
    /// <summary>Reads the field from a struct value.</summary>
    member _.Get = get
    /// <summary>The field's codec.</summary>
    member _.Codec = codec

/// <summary>The fields of a struct that are still to be read, and then the value they build.</summary>
/// <remarks>A <c>bareStruct</c> expression produces these; nothing else needs to.</remarks>
[<AbstractClass>]
type BareStruct<'R>() =
    /// <summary>Reads the remaining fields at an offset and builds the value, with the next offset or <c>Cursor.Fault</c>.</summary>
    abstract Read: data: byte array * offset: int -> 'R * int

/// <summary>A struct with a field still to read. A class, not an interface, so Fable can test for it.</summary>
[<AbstractClass>]
type internal BareStructField<'R>() =
    inherit BareStruct<'R>()
    abstract Field: StructField
    abstract Declarations: NamedType array
    abstract Size: 'R -> int
    abstract Write: data: byte array * offset: int * value: 'R -> int
    /// <summary>The rest of the struct as it is with a default value for this field.</summary>
    abstract Rest: unit -> BareStruct<'R>

type internal BareStructBind<'R, 'F>(field: BareField<'R, 'F>, rest: 'F -> BareStruct<'R>) =
    inherit BareStructField<'R>()

    override _.Read(data, offset) =
        let value, next = field.Codec.Read data offset

        if Cursor.isOk next then
            (rest value).Read(data, next)
        else
            (Unchecked.defaultof<'R>, Cursor.Fault)

    override _.Field = Bare.field field.Name field.Codec.Schema
    override _.Declarations = field.Codec.Declarations

    override _.Size value =
        field.Codec.Size (field.Get value)

    override _.Write(data, offset, value) =
        field.Codec.Write data offset (field.Get value)

    override _.Rest() =
        rest Unchecked.defaultof<'F>

type internal BareStructReturn<'R>(value: 'R) =
    inherit BareStruct<'R>()

    override _.Read(_, offset) =
        if Cursor.isOk offset then
            (value, offset)
        else
            (Unchecked.defaultof<'R>, Cursor.Fault)

/// <summary>
/// Builds a struct codec: each <c>let!</c> binds a <c>BareCodec.field</c> in wire order, and <c>return</c> builds the
/// value from the fields.
/// </summary>
/// <remarks>
/// The expression runs once with a default value for every field when the codec is built, to learn the fields and
/// their order, and once per decoded value. So it holds only <c>let!</c> bindings of fields and a final
/// <c>return</c>: it must bind the same fields whatever their values, and must not raise on a null or zero field.
/// Convert or check a field with <c>BareCodec.convert</c> or <c>BareCodec.refine</c> on its codec.
/// </remarks>
[<Sealed>]
type BareStructBuilder() =

    /// <summary>A field, then the rest of the struct given its value.</summary>
    member _.Bind(field: BareField<'R, 'F>, rest: 'F -> BareStruct<'R>) : BareStruct<'R> =
        BareStructBind<'R, 'F>(field, rest) :> BareStruct<'R>

    /// <summary>The struct value, once every field is bound.</summary>
    member _.Return(value: 'R) : BareStruct<'R> =
        BareStructReturn<'R> value :> BareStruct<'R>

    /// <summary>
    /// The codec of the struct. Raises <c>InvalidOperationException</c> when the expression binds no field, or
    /// raises while it runs with default field values.
    /// </summary>
    member _.Run(first: BareStruct<'R>) : BareCodec<'R> =
        let steps = ResizeArray<BareStructField<'R>>()
        let mutable node = first
        let mutable described = false

        while not described do
            match node with
            | :? BareStructField<'R> as step ->
                steps.Add step

                node <-
                    try
                        step.Rest ()
                    with error ->
                        raise (
                            System.InvalidOperationException (
                                "The bareStruct expression raised after binding the field "
                                + step.Field.Name
                                + " with a default value. It runs once with default field values when the codec is built, "
                                + "so it must only bind fields and return the value.",
                                error
                            )
                        )
            | _ -> described <- true

        if steps.Count = 0 then
            invalidOp "A bareStruct expression binds at least one field: BARE has no empty struct."

        let fields = steps.ToArray ()
        let mutable declarations: NamedType array = Array.empty

        for step in fields do
            declarations <- Wire.mergeDeclarations declarations step.Declarations

        {
            Schema = Bare.struct' (fields |> Array.map (fun step -> step.Field))
            Declarations = declarations
            Size =
                fun value ->
                    let mutable total = 0
                    let mutable i = 0

                    while i < fields.Length && total >= 0 do
                        total <- Wire.add total (fields.[i].Size value)
                        i <- i + 1

                    total
            Write =
                fun data offset value ->
                    let mutable position = offset
                    let mutable i = 0

                    while i < fields.Length && Cursor.isOk position do
                        position <- fields.[i].Write(data, position, value)
                        i <- i + 1

                    position
            Read = fun data offset -> first.Read (data, offset)
        }

/// <summary>The codecs of BARE's types, over BAREWire's encoder and decoder.</summary>
[<RequireQualifiedAccess>]
module BareCodec =

    let private faulted<'T> : 'T * int = (Unchecked.defaultof<'T>, Cursor.Fault)

    /// <summary>
    /// A codec from a writer and reader in BAREWire's shape, the exact size of a value, and the BARE type the pair
    /// implements.
    /// </summary>
    let create (schema: SchemaType) (size: 'T -> int) (write: byte array -> int -> 'T -> int) (read: byte array -> int -> 'T * int) : BareCodec<'T> =
        {
            Schema = schema
            Declarations = Array.empty
            Size = size
            Write = write
            Read = read
        }

    /// <summary>
    /// A codec from a BAREWire writer and reader with a declared capacity, as <c>BAREWire.Encoding.Codec.encode</c>
    /// takes them. A value is sized by writing it once into a buffer of the capacity; a value that does not fit has
    /// no encoding. Raises <c>ArgumentException</c> for a negative capacity.
    /// </summary>
    let ofBareWire
        (schema: SchemaType)
        (capacity: int)
        (write: byte array -> int -> 'T -> int)
        (read: byte array -> int -> 'T * int)
        : BareCodec<'T> =
        if capacity < 0 then
            invalidArg
                "capacity"
                ("The capacity must not be negative; it is "
                 + string capacity
                 + ".")

        create schema (fun value -> write (Array.zeroCreate capacity) 0 value) write read

    /// <summary>
    /// The codec under a type name: its schema becomes a reference to <c>name</c>, declared as the codec's type.
    /// </summary>
    let named (name: string) (codec: BareCodec<'T>) : BareCodec<'T> =
        let declaration: NamedType =
            {
                NamedType.Name = name
                NamedType.Type = codec.Schema
            }

        { codec with
            Schema = Bare.typeRef name
            Declarations = Wire.mergeDeclarations codec.Declarations [| declaration |]
        }

    /// <summary>
    /// The codec of a type held on the wire as another: <c>ofWire</c> runs on a decoded value, <c>toWire</c> on a
    /// value to size and to write.
    /// </summary>
    let convert (ofWire: 'A -> 'B) (toWire: 'B -> 'A) (codec: BareCodec<'A>) : BareCodec<'B> =
        {
            Schema = codec.Schema
            Declarations = codec.Declarations
            Size = fun value -> codec.Size (toWire value)
            Write = fun data offset value -> codec.Write data offset (toWire value)
            Read =
                fun data offset ->
                    let wire, next = codec.Read data offset

                    if Cursor.isOk next then (ofWire wire, next) else faulted
        }

    /// <summary>
    /// As <c>convert</c>, for a type that holds only some wire values: a decoded value <c>ofWire</c> answers
    /// <c>ValueNone</c> for is a fault.
    /// </summary>
    let refine (ofWire: 'A -> 'B voption) (toWire: 'B -> 'A) (codec: BareCodec<'A>) : BareCodec<'B> =
        {
            Schema = codec.Schema
            Declarations = codec.Declarations
            Size = fun value -> codec.Size (toWire value)
            Write = fun data offset value -> codec.Write data offset (toWire value)
            Read =
                fun data offset ->
                    let wire, next = codec.Read data offset

                    if Cursor.isOk next then
                        match ofWire wire with
                        | ValueSome value -> (value, next)
                        | ValueNone -> faulted
                    else
                        faulted
        }

    /// <summary>A value that may be absent: one tag byte, then the value when present.</summary>
    let option (inner: BareCodec<'T>) : BareCodec<'T option> =
        {
            Schema = Bare.optional inner.Schema
            Declarations = inner.Declarations
            Size =
                fun value ->
                    match value with
                    | None -> 1
                    | Some present -> Wire.add 1 (inner.Size present)
            Write = fun data offset value -> Encoder.writeOption data offset inner.Write value
            Read = fun data offset -> Decoder.readOption data offset inner.Read
        }

    /// <summary>A variable-length list as an array: a <c>uint</c> count, then the values. A null array has no encoding.</summary>
    let array (inner: BareCodec<'T>) : BareCodec<'T array> =
        {
            Schema = Bare.list inner.Schema
            Declarations = inner.Declarations
            Size =
                fun items ->
                    if isNull items then
                        Cursor.Fault
                    else
                        let mutable total = Wire.uintSize (uint64 items.Length)
                        let mutable i = 0

                        while i < items.Length && total >= 0 do
                            total <- Wire.add total (inner.Size items.[i])
                            i <- i + 1

                        total
            Write =
                fun data offset items ->
                    if isNull items then
                        Cursor.Fault
                    else
                        Encoder.writeList data offset inner.Write items
            Read = fun data offset -> Decoder.readList data offset inner.Read
        }

    /// <summary>A variable-length list as an F# list.</summary>
    let list (inner: BareCodec<'T>) : BareCodec<'T list> =
        convert List.ofArray List.toArray (array inner)

    /// <summary>
    /// A list of exactly <c>length</c> values, with no count on the wire. An array of another length has no encoding.
    /// Raises <c>ArgumentException</c> when <c>length</c> is below 1.
    /// </summary>
    let fixedArray (length: int) (inner: BareCodec<'T>) : BareCodec<'T array> =
        if length < 1 then
            invalidArg
                "length"
                ("A fixed list holds at least 1 value; the length is "
                 + string length
                 + ".")

        {
            Schema = Bare.fixedList inner.Schema length
            Declarations = inner.Declarations
            Size =
                fun items ->
                    if isNull items || items.Length <> length then
                        Cursor.Fault
                    else
                        let mutable total = 0
                        let mutable i = 0

                        while i < length && total >= 0 do
                            total <- Wire.add total (inner.Size items.[i])
                            i <- i + 1

                        total
            Write =
                fun data offset items ->
                    if isNull items then
                        Cursor.Fault
                    else
                        Encoder.writeFixedList data offset inner.Write items length
            Read = fun data offset -> Decoder.readFixedList data offset length inner.Read
        }

    /// <summary>
    /// A map: a <c>uint</c> count, then each key and its value, written in the keys' order. A repeated key on the
    /// wire is a fault. BARE keys are primitives other than <c>f32</c>, <c>f64</c>, <c>data</c> and <c>void</c>;
    /// <c>BareCodec.validate</c> reports any other key type.
    /// </summary>
    let map (key: BareCodec<'K>) (value: BareCodec<'V>) : BareCodec<Map<'K, 'V>> =
        {
            Schema = Bare.map key.Schema value.Schema
            Declarations = Wire.mergeDeclarations key.Declarations value.Declarations
            Size =
                fun entries ->
                    let mutable total = Wire.uintSize (uint64 entries.Count)

                    for KeyValue (k, v) in entries do
                        total <- Wire.add total (Wire.add (key.Size k) (value.Size v))

                    total
            Write =
                fun data offset entries ->
                    let mutable position = Encoder.writeUInt data offset (uint64 entries.Count)

                    for KeyValue (k, v) in entries do
                        position <- value.Write data (key.Write data position k) v

                    position
            Read =
                fun data offset ->
                    let count, start = Decoder.readUInt data offset

                    if
                        Cursor.isFault start
                        || count > uint64 (Cursor.remaining data start)
                    then
                        faulted
                    else
                        let mutable entries = Map.empty
                        let mutable position = start
                        let mutable remaining = count

                        while remaining > 0UL && Cursor.isOk position do
                            let k, afterKey = key.Read data position
                            let v, afterValue = value.Read data afterKey

                            if
                                Cursor.isOk afterValue
                                && not (entries.ContainsKey k)
                            then
                                entries <- entries.Add (k, v)
                                position <- afterValue
                            else
                                position <- Cursor.Fault

                            remaining <- remaining - 1UL

                        if Cursor.isOk position then
                            (entries, position)
                        else
                            faulted
        }

    /// <summary>
    /// An enum declared under <c>name</c>: each value's schema name, wire value and F# value. A value outside the
    /// list has no encoding, and a wire value outside it is a fault. Raises <c>ArgumentException</c> for an empty list.
    /// </summary>
    let enum (name: string) (values: (string * uint64 * 'T) list) : BareCodec<'T> when 'T: equality =
        if List.isEmpty values then
            invalidArg "values" ("The enum " + name + " declares no value.")

        let table = List.toArray values

        let wireOf (value: 'T) : int =
            let mutable found = -1
            let mutable i = 0

            while found < 0 && i < table.Length do
                let _, _, candidate = table.[i]

                if candidate = value then
                    found <- i

                i <- i + 1

            found

        let declaration: NamedType =
            {
                NamedType.Name = name
                NamedType.Type =
                    Bare.enum'
                        PrimKind.UInt
                        (table
                         |> Array.map (fun (label, wire, _) -> Bare.enumValue label wire))
            }

        {
            Schema = Bare.typeRef name
            Declarations = [| declaration |]
            Size =
                fun value ->
                    let index = wireOf value

                    if index < 0 then
                        Cursor.Fault
                    else
                        let _, wire, _ = table.[index]
                        Wire.uintSize wire
            Write =
                fun data offset value ->
                    let index = wireOf value

                    if index < 0 then
                        Cursor.Fault
                    else
                        let _, wire, _ = table.[index]
                        Encoder.writeUInt data offset wire
            Read =
                fun data offset ->
                    let wire, next = Decoder.readUInt data offset
                    let mutable found = -1
                    let mutable i = 0

                    while found < 0 && i < table.Length do
                        let _, candidate, _ = table.[i]

                        if candidate = wire then
                            found <- i

                        i <- i + 1

                    if Cursor.isOk next && found >= 0 then
                        let _, _, value = table.[found]
                        (value, next)
                    else
                        faulted
        }

    /// <summary>
    /// A union case carrying a value: <c>inject</c> builds the union value from a decoded payload, and
    /// <c>project</c> answers the payload of a value of this case and <c>ValueNone</c> for any other.
    /// </summary>
    /// <remarks>
    /// BARE tells cases apart by type, so two cases of one union carry different types: give cases of the same shape
    /// their own names with <c>BareCodec.named</c>.
    /// </remarks>
    let case (tag: int) (codec: BareCodec<'T>) (inject: 'T -> 'U) (project: 'U -> 'T voption) : BareCase<'U> =
        {
            Tag = tag
            CaseSchema = codec.Schema
            CaseDeclarations = codec.Declarations
            CaseSize =
                fun value ->
                    match project value with
                    | ValueSome payload -> Wire.add (Wire.uintSize (uint64 tag)) (codec.Size payload)
                    | ValueNone -> Wire.NotThisCase
            CaseWrite =
                fun data offset value ->
                    match project value with
                    | ValueSome payload -> codec.Write data (Encoder.writeTag data offset tag) payload
                    | ValueNone -> Wire.NotThisCase
            CaseRead =
                fun data offset ->
                    let payload, next = codec.Read data offset

                    if Cursor.isOk next then (inject payload, next) else faulted
        }

    /// <summary>
    /// A union case carrying nothing, declared as the <c>void</c> type <c>name</c>: <c>value</c> is the union value,
    /// and <c>isCase</c> answers whether a value is this case.
    /// </summary>
    let caseVoid (tag: int) (name: string) (value: 'U) (isCase: 'U -> bool) : BareCase<'U> =
        let declaration: NamedType =
            {
                NamedType.Name = name
                NamedType.Type = Bare.void'
            }

        {
            Tag = tag
            CaseSchema = Bare.typeRef name
            CaseDeclarations = [| declaration |]
            CaseSize =
                fun candidate ->
                    if isCase candidate then
                        Wire.uintSize (uint64 tag)
                    else
                        Wire.NotThisCase
            CaseWrite =
                fun data offset candidate ->
                    if isCase candidate then
                        Encoder.writeTag data offset tag
                    else
                        Wire.NotThisCase
            CaseRead = fun _ offset -> if Cursor.isOk offset then (value, offset) else faulted
        }

    /// <summary>
    /// A tagged union: a <c>uint</c> tag, then the value of the case with that tag. A value no case claims has no
    /// encoding, and a tag no case declares is a fault. Raises <c>ArgumentException</c> for an empty list, a
    /// negative tag or a repeated tag.
    /// </summary>
    let union (cases: BareCase<'U> list) : BareCodec<'U> =
        if List.isEmpty cases then
            invalidArg "cases" "A union declares at least one case."

        let table = List.toArray cases

        for i = 0 to table.Length - 1 do
            if table.[i].Tag < 0 then
                invalidArg
                    "cases"
                    ("A union tag must not be negative; one is "
                     + string table.[i].Tag
                     + ".")

            for j = 0 to i - 1 do
                if table.[j].Tag = table.[i].Tag then
                    invalidArg
                        "cases"
                        ("The union tag "
                         + string table.[i].Tag
                         + " is declared twice.")

        let mutable declarations: NamedType array = Array.empty

        for entry in table do
            declarations <- Wire.mergeDeclarations declarations entry.CaseDeclarations

        {
            Schema =
                Bare.union (
                    table
                    |> Array.map (fun entry -> Bare.case entry.Tag entry.CaseSchema)
                )
            Declarations = declarations
            Size =
                fun value ->
                    let mutable size = Wire.NotThisCase
                    let mutable i = 0

                    while size = Wire.NotThisCase && i < table.Length do
                        size <- table.[i].CaseSize value
                        i <- i + 1

                    if size = Wire.NotThisCase then Cursor.Fault else size
            Write =
                fun data offset value ->
                    let mutable next = Wire.NotThisCase
                    let mutable i = 0

                    while next = Wire.NotThisCase && i < table.Length do
                        next <- table.[i].CaseWrite data offset value
                        i <- i + 1

                    if next = Wire.NotThisCase then Cursor.Fault else next
            Read =
                fun data offset ->
                    let tag, afterTag = Decoder.readTag data offset
                    let mutable found = -1
                    let mutable i = 0

                    while found < 0 && i < table.Length do
                        if table.[i].Tag = tag then
                            found <- i

                        i <- i + 1

                    if Cursor.isOk afterTag && found >= 0 then
                        table.[found].CaseRead data afterTag
                    else
                        faulted
        }

    /// <summary>A struct field: its schema name, how to read it from the struct value, and its codec.</summary>
    let field (name: string) (get: 'R -> 'F) (codec: BareCodec<'F>) : BareField<'R, 'F> =
        BareField<'R, 'F>(name, get, codec)

    /// <summary>
    /// The schema of a codec with <c>root</c> as its root type: the codec's declarations, then <c>root</c> declared
    /// as the codec's type unless the codec is already named <c>root</c>.
    /// </summary>
    let schema (root: string) (codec: BareCodec<'T>) : SchemaDefinition =
        match codec.Schema with
        | TypeRef name when name = root ->
            {
                Types = codec.Declarations
                Root = root
            }
        | _ ->
            let declaration: NamedType =
                {
                    NamedType.Name = root
                    NamedType.Type = codec.Schema
                }

            {
                Types = Wire.mergeDeclarations codec.Declarations [| declaration |]
                Root = root
            }

    /// <summary>
    /// BAREWire's verdict on the codec's schema; empty means well-formed. Reports what the combinators cannot refuse
    /// when a codec is built: two union cases of one type, a map key BARE cannot key by, one name declared as two
    /// types, a repeated field or enum name.
    /// </summary>
    let validate (root: string) (codec: BareCodec<'T>) : ValidationError array =
        Validation.validate (schema root codec)

    /// <summary>The codec's schema as BARE schema text, as BAREWire emits it.</summary>
    let schemaText (root: string) (codec: BareCodec<'T>) : string =
        Emit.schema (schema root codec)

    /// <summary>
    /// BAREWire's size bounds for the codec's type within its schema rooted at <c>root</c>, or BAREWire's findings
    /// for a schema that is not well-formed.
    /// </summary>
    let wireSize (root: string) (codec: BareCodec<'T>) : Result<Size, ValidationError array> =
        Analysis.wireSize (schema root codec) codec.Schema

    /// <summary>BARE <c>u8</c>.</summary>
    let u8: BareCodec<byte> = create Bare.u8 (fun _ -> 1) Encoder.writeU8 Decoder.readU8

    /// <summary>BARE <c>u16</c>, little-endian.</summary>
    let u16: BareCodec<uint16> =
        create Bare.u16 (fun _ -> 2) Encoder.writeU16 Decoder.readU16

    /// <summary>BARE <c>u32</c>, little-endian.</summary>
    let u32: BareCodec<uint32> =
        create Bare.u32 (fun _ -> 4) Encoder.writeU32 Decoder.readU32

    /// <summary>BARE <c>u64</c>, little-endian.</summary>
    let u64: BareCodec<uint64> =
        create Bare.u64 (fun _ -> 8) Encoder.writeU64 Decoder.readU64

    /// <summary>BARE <c>i8</c>.</summary>
    let i8: BareCodec<sbyte> =
        create Bare.i8 (fun _ -> 1) Encoder.writeI8 Decoder.readI8

    /// <summary>BARE <c>i16</c>, little-endian two's complement.</summary>
    let i16: BareCodec<int16> =
        create Bare.i16 (fun _ -> 2) Encoder.writeI16 Decoder.readI16

    /// <summary>BARE <c>i32</c>, little-endian two's complement.</summary>
    let i32: BareCodec<int32> =
        create Bare.i32 (fun _ -> 4) Encoder.writeI32 Decoder.readI32

    /// <summary>BARE <c>i64</c>, little-endian two's complement.</summary>
    let i64: BareCodec<int64> =
        create Bare.i64 (fun _ -> 8) Encoder.writeI64 Decoder.readI64

    /// <summary>BARE <c>f32</c>, IEEE-754 binary32; a NaN keeps its bits.</summary>
    let f32: BareCodec<float32> =
        create Bare.f32 (fun _ -> 4) Encoder.writeF32 Decoder.readF32

    /// <summary>BARE <c>f64</c>, IEEE-754 binary64; a NaN keeps its bits.</summary>
    let f64: BareCodec<float> =
        create Bare.f64 (fun _ -> 8) Encoder.writeF64 Decoder.readF64

    /// <summary>BARE <c>bool</c>: one byte, 0 or 1; any other byte is a fault.</summary>
    let bool: BareCodec<bool> =
        create Bare.bool (fun _ -> 1) Encoder.writeBool Decoder.readBool

    /// <summary>BARE <c>data</c>: a <c>uint</c> length, then the bytes. A null array has no encoding.</summary>
    let data: BareCodec<byte array> =
        create
            Bare.data
            (fun bytes ->
                if isNull bytes then
                    Cursor.Fault
                else
                    Wire.prefixed bytes.Length)
            (fun buffer offset bytes ->
                if isNull bytes then
                    Cursor.Fault
                else
                    Encoder.writeData buffer offset bytes)
            Decoder.readData

    /// <summary>
    /// BARE <c>data[length]</c>: exactly <c>length</c> bytes with no prefix. An array of another length has no
    /// encoding. Raises <c>ArgumentException</c> when <c>length</c> is below 1.
    /// </summary>
    let fixedData (length: int) : BareCodec<byte array> =
        if length < 1 then
            invalidArg
                "length"
                ("Fixed data holds at least 1 byte; the length is "
                 + string length
                 + ".")

        create
            (Bare.fixedData length)
            (fun bytes ->
                if isNull bytes || bytes.Length <> length then
                    Cursor.Fault
                else
                    length)
            (fun buffer offset bytes ->
                if isNull bytes then
                    Cursor.Fault
                else
                    Encoder.writeFixedData buffer offset bytes length)
            (fun buffer offset -> Decoder.readFixedData buffer offset length)

    /// <summary>
    /// BARE <c>uint</c> as an F# <c>uint32</c>: a varint of one to five bytes. A wire value above
    /// <c>UInt32.MaxValue</c> is a fault.
    /// </summary>
    let uint32: BareCodec<uint32> =
        create
            Bare.uint
            (fun value -> Wire.uintSize (uint64 value))
            (fun buffer offset value -> Encoder.writeUInt buffer offset (uint64 value))
            (fun buffer offset ->
                let wide, next = Decoder.readUInt buffer offset

                if Cursor.isOk next && wide <= 4294967295UL then
                    (Operators.uint32 wide, next)
                else
                    (0u, Cursor.Fault))

    /// <summary>
    /// BARE <c>int</c> as an F# <c>int</c>: a zigzag varint of one to five bytes. A wire value outside the
    /// <c>int32</c> range is a fault.
    /// </summary>
    let int32: BareCodec<int> =
        create
            Bare.int
            (fun value -> Wire.uintSize (Wire.zigzag (int64 value)))
            (fun buffer offset value -> Encoder.writeInt buffer offset (int64 value))
            (fun buffer offset ->
                let wide, next = Decoder.readInt buffer offset

                if
                    Cursor.isOk next
                    && wide >= -2147483648L
                    && wide <= 2147483647L
                then
                    (Operators.int wide, next)
                else
                    (0, Cursor.Fault))

    /// <summary>BARE <c>uint</c>: a varint of one to ten bytes.</summary>
    let uint: BareCodec<uint64> =
        create Bare.uint Wire.uintSize Encoder.writeUInt Decoder.readUInt

    /// <summary>BARE <c>int</c>: a zigzag varint of one to ten bytes.</summary>
    let int: BareCodec<int64> =
        create Bare.int (fun value -> Wire.uintSize (Wire.zigzag value)) Encoder.writeInt Decoder.readInt

    /// <summary>
    /// BARE <c>str</c>: a <c>uint</c> byte length, then UTF-8. A null string has no encoding. BAREWire writes an
    /// unpaired surrogate as U+FFFD and reads ill-formed UTF-8 as U+FFFD, so neither is a fault.
    /// </summary>
    let string: BareCodec<string> =
        create
            Bare.string
            (fun text ->
                if isNull text then
                    Cursor.Fault
                else
                    Wire.prefixed (Wire.utf8Length text))
            (fun buffer offset text ->
                if isNull text then
                    Cursor.Fault
                else
                    Encoder.writeString buffer offset text)
            Decoder.readString

/// <summary>The <c>bareStruct</c> computation expression.</summary>
[<AutoOpen>]
module BareStruct =

    /// <summary>
    /// Builds a struct codec: each <c>let!</c> binds a <c>BareCodec.field</c> in wire order, and <c>return</c> builds
    /// the value from the fields. The expression runs once with default field values when the codec is built.
    /// </summary>
    let bareStruct = BareStructBuilder ()

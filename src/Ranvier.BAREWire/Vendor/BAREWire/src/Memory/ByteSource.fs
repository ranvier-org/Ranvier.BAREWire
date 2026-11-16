namespace BAREWire.Memory

/// Representation-access errors carry no compiler or execution authority.
[<RequireQualifiedAccess>]
type SourceError =
    | OutOfBounds
    | Unavailable of reason: string

/// A read-only host adapter, never part of a published semantic value. The host
/// owns its lifetime and must keep its bytes stable for every reader using it.
/// A reader does not acquire, map, close or otherwise operate the host resource.
type ByteSource = private {
    Extent: uint64
    ReadRange: uint64 -> int -> byte array option
}

module ByteSource =
    /// Supply a bounded reader over stable bytes. Return None after the source
    /// becomes unavailable. Each successful read must return exactly count bytes.
    let create length readRange = { Extent = length; ReadRange = readRange }

    /// Own a copy so later writes by the caller cannot change this source.
    let ofArray (bytes: byte array) =
        if isNull bytes then create 0UL (fun _ _ -> None)
        else
            let owned = Array.copy bytes
            create (uint64 owned.Length) (fun offset count -> Some(Array.sub owned (int offset) count))

    let length source = source.Extent

    /// Validate bounds before invoking host code; refuse short/null reads and
    /// turn disposal or other host access failures into a typed result.
    let read offset count source =
        if count < 0 || offset > source.Extent || uint64 count > source.Extent - offset then
            Error SourceError.OutOfBounds
        else
            try
                match source.ReadRange offset count with
                | Some bytes when not (isNull bytes) && bytes.Length = count -> Ok bytes
                | _ -> Error(SourceError.Unavailable "The byte source did not provide the exact requested extent.")
            with error -> Error(SourceError.Unavailable error.Message)

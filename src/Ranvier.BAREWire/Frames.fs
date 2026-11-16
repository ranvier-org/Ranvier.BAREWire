namespace Ranvier.BAREWire

open BAREWire.Encoding
open BAREWire.Framing

/// <summary>BAREWire envelope frames with typed payloads: kind byte, <c>u32</c> correlation id, BARE payload.</summary>
[<RequireQualifiedAccess>]
module BareFrame =

    /// <summary>The kind's name, or its byte for a kind BAREWire does not declare.</summary>
    let kindName (kind: FrameKind) : string =
        if kind = FrameKind.Tell then "Tell"
        elif kind = FrameKind.Ask then "Ask"
        elif kind = FrameKind.Reply then "Reply"
        elif kind = FrameKind.Event then "Event"
        elif kind = FrameKind.Control then "Control"
        else "kind " + string kind

    /// <summary>An Ask frame carrying the value; the Reply echoes <c>correlation</c>. Raises as <c>Encode</c> does.</summary>
    let ask (correlation: uint32) (codec: BareCodec<'T>) (value: 'T) : Frame =
        Envelope.ask correlation (codec.Encode value)

    /// <summary>A Reply frame carrying the value, answering the Ask with <c>correlation</c>.</summary>
    let reply (correlation: uint32) (codec: BareCodec<'T>) (value: 'T) : Frame =
        Envelope.reply correlation (codec.Encode value)

    /// <summary>A Tell frame carrying the value.</summary>
    let tell (codec: BareCodec<'T>) (value: 'T) : Frame =
        Envelope.tell (codec.Encode value)

    /// <summary>An Event frame carrying the value.</summary>
    let event (codec: BareCodec<'T>) (value: 'T) : Frame =
        Envelope.event (codec.Encode value)

    /// <summary>The frame's bytes for a transport that frames messages itself, as a WebSocket binary message.</summary>
    let encode (frame: Frame) : byte array =
        Envelope.encodeMessage frame

    /// <summary>
    /// The frame a self-framed message holds, or <c>BareError.Frame</c> for null bytes, a header shorter than five
    /// bytes or an unknown kind byte.
    /// </summary>
    let decode (bytes: byte array) : Result<Frame, BareError> =
        if isNull bytes then
            Error (BareError.Frame "The frame bytes are null.")
        else
            let frame, payloadAt = Envelope.decodeMessage bytes

            if Cursor.isOk payloadAt then
                Ok frame
            else
                Error (
                    BareError.Frame (
                        "The "
                        + string bytes.Length
                        + " bytes are not a BAREWire frame: the header is five bytes and the kind byte is 0 to 4."
                    )
                )

    /// <summary>The value the frame's payload encodes.</summary>
    let payload (codec: BareCodec<'T>) (frame: Frame) : Result<'T, BareError> =
        codec.Decode frame.Payload

namespace BAREWire.Platform

open BAREWire.Encoding

/// One consistency finding about a description: the declaration it is about,
/// the kind of problem, and a sentence a person can act on.
type Finding = {
    Subject: string
    Kind: string
    Message: string
}

/// The kinds of finding `Check.run` reports.
[<RequireQualifiedAccess>]
module FindingKind =
    [<Literal>]
    let SharedBufferLayoutInvalid: string = "FS9220"
    [<Literal>]
    let SharedBufferExtentInvalid: string = "FS9221"
    [<Literal>]
    let SharedBufferLifetimeInvalid: string = "FS9222"
    [<Literal>]
    let SharedBufferConflictingAccess: string = "FS9223"
    [<Literal>]
    let InvalidLinkArtifact: string = "invalid-link-artifact"
    [<Literal>]
    let InvalidArchiveDigest: string = "invalid-archive-digest"
    [<Literal>]
    let InvalidLinkPolicy: string = "invalid-link-policy"
    [<Literal>]
    let UnknownSpace: string = "unknown-space"
    [<Literal>]
    let CapacityExceedsSpace: string = "capacity-exceeds-space"
    [<Literal>]
    let NonPositiveCapacity: string = "non-positive-capacity"
    [<Literal>]
    let AlignmentNotPowerOfTwo: string = "alignment-not-power-of-two"
    [<Literal>]
    let OverlappingSpaces: string = "overlapping-spaces"
    [<Literal>]
    let MissingDelimiter: string = "missing-delimiter"
    [<Literal>]
    let UnknownEndpoint: string = "unknown-endpoint"
    [<Literal>]
    let DuplicateName: string = "duplicate-name"
    [<Literal>]
    let EmptyName: string = "empty-name"
    [<Literal>]
    let UnknownTag: string = "unknown-tag"
    [<Literal>]
    let InvalidAvailability: string = "invalid-availability"
    [<Literal>]
    let PointerInSharedLayout: string = "pointer-in-shared-layout"
    [<Literal>]
    let NegativeLimit: string = "negative-limit"
    [<Literal>]
    let InvalidSlot: string = "invalid-slot"
    [<Literal>]
    let NegativeRate: string = "negative-rate"
    [<Literal>]
    let NonPositiveFrequency: string = "non-positive-frequency"
    [<Literal>]
    let BaseMisaligned: string = "base-misaligned"
    [<Literal>]
    let NonPositiveWidth: string = "non-positive-width"
    [<Literal>]
    let MissingRange: string = "missing-range"
    [<Literal>]
    let InvalidRange: string = "invalid-range"
    [<Literal>]
    let WidthMismatch: string = "width-mismatch"
    [<Literal>]
    let InvalidProgramLifetimeAccess: string = "invalid-program-lifetime-access"

/// The consistency check on a description (docs/11): every tag is one the
/// vocabulary names, every name reference resolves, capacities are positive
/// and fit their spaces, alignments are powers of two, spaces with declared
/// bases do not overlap, and a delimited buffer declares its delimiter. An
/// empty result means the description is consistent. The check is total and
/// deterministic: findings come out in declaration order.
module Check =

    let private push (acc: Finding array) (f: Finding) : Finding array =
        let n = Array.length acc
        let m = n + 1
        let out : Finding array = Array.zeroCreate m
        let mutable i = 0
        while i < n do
            Array.set out i (Array.get acc i)
            i <- i + 1
        Array.set out n f
        out

    let private finding (subject: string) (kind: string) (message: string) : Finding =
        { Subject = subject; Kind = kind; Message = message }

    let private isPowerOfTwo (v: int) : bool =
        v > 0 && (v &&& (v - 1)) = 0

    let private dotted (a: string) (b: string) : string =
        Text.append (Text.append a ".") b

    /// Report an empty name for a declaration of the given role.
    let private checkName (acc: Finding array) (role: string) (name: string) : Finding array =
        if String.length name = 0 then
            push acc (finding role FindingKind.EmptyName (Text.append role " has an empty name"))
        else acc

    /// Report a tag the vocabulary does not name.
    let private checkTag (acc: Finding array) (subject: string) (field: string) (valid: bool) (value: string) : Finding array =
        if valid then acc
        else
            let msg = Text.append (Text.append (Text.append field " '") value) "' is not a tag the vocabulary names"
            push acc (finding subject FindingKind.UnknownTag msg)

    let private checkContract (acc: Finding array) (owner: string) (c: Contract) : Finding array =
        let subject = dotted owner c.Name
        let acc1 = checkName acc (Text.append owner " contract") c.Name
        checkTag acc1 subject "logic" (Logic.isValid c.Logic) c.Logic

    let private checkContracts (acc: Finding array) (owner: string) (contracts: Contract array) : Finding array =
        let n = Array.length contracts
        let mutable out = acc
        let mutable i = 0
        while i < n do
            out <- checkContract out owner (Array.get contracts i)
            i <- i + 1
        out

    /// Report a name that an earlier declaration in the same table already used.
    let private checkDuplicate (acc: Finding array) (role: string) (name: string) (seenBefore: bool) : Finding array =
        if seenBefore then
            push acc (finding name FindingKind.DuplicateName (Text.append (Text.append role " '") (Text.append name "' is declared more than once")))
        else acc

    let private spaceSeenBefore (spaces: MemorySpace array) (upto: int) (name: string) : bool =
        let mutable seen = false
        let mutable j = 0
        while not seen && j < upto do
            seen <- (Array.get spaces j).Name = name
            j <- j + 1
        seen

    let private bufferSeenBefore (buffers: BufferSchema array) (upto: int) (name: string) : bool =
        let mutable seen = false
        let mutable j = 0
        while not seen && j < upto do
            seen <- (Array.get buffers j).Name = name
            j <- j + 1
        seen

    let private surfaceSeenBefore (surfaces: BoundarySurface array) (upto: int) (name: string) : bool =
        let mutable seen = false
        let mutable j = 0
        while not seen && j < upto do
            seen <- (Array.get surfaces j).Name = name
            j <- j + 1
        seen

    let private indexOfEndpointIn (endpoints: Endpoint array) (name: string) : int =
        let n = Array.length endpoints
        let mutable i = 0
        let mutable found = -1
        while found < 0 && i < n do
            if (Array.get endpoints i).Name = name then found <- i
            i <- i + 1
        found

    let private endpointSeenBefore (endpoints: Endpoint array) (upto: int) (name: string) : bool =
        let mutable seen = false
        let mutable j = 0
        while not seen && j < upto do
            seen <- (Array.get endpoints j).Name = name
            j <- j + 1
        seen

    let private transportSeenBefore (transports: Transport array) (upto: int) (name: string) : bool =
        let mutable seen = false
        let mutable j = 0
        while not seen && j < upto do
            seen <- (Array.get transports j).Name = name
            j <- j + 1
        seen

    let private checkSpace (acc: Finding array) (spaces: MemorySpace array) (index: int) : Finding array =
        let s = Array.get spaces index
        let acc1 = checkName acc "space" s.Name
        let acc2 = checkDuplicate acc1 "space" s.Name (spaceSeenBefore spaces index s.Name)
        let acc3 = checkTag acc2 s.Name "kind" (MemoryKind.isValid s.Kind) s.Kind
        let acc4 = checkTag acc3 s.Name "growth" (Growth.isValid s.Growth) s.Growth
        let acc5 = checkTag acc4 s.Name "access" (Access.isValid s.Access) s.Access
        let acc6 =
            if s.Capacity <= 0L then
                push acc5 (finding s.Name FindingKind.NonPositiveCapacity (Text.append "space capacity is not positive: " (Fmt.ofInt64 s.Capacity)))
            else acc5
        let acc7 =
            if isPowerOfTwo s.Alignment then acc6
            else push acc6 (finding s.Name FindingKind.AlignmentNotPowerOfTwo (Text.append "space alignment is not a power of two: " (Fmt.ofInt s.Alignment)))
        let acc8 =
            if isPowerOfTwo s.Granularity then acc7
            else push acc7 (finding s.Name FindingKind.AlignmentNotPowerOfTwo (Text.append "space granularity is not a power of two: " (Fmt.ofInt s.Granularity)))
        let acc9 =
            if s.Kind = MemoryKind.Map then checkTag acc8 s.Name "map kind" (MapKind.isValid s.MapKind) s.MapKind
            elif String.length s.MapKind > 0 then push acc8 (finding s.Name FindingKind.UnknownTag "map kind declared on a space that is not a map")
            else acc8
        let baseValue =
            match s.Base with
            | Some b -> b
            | None -> 0L
        let acc10 =
            if s.Alignment > 0 && baseValue % int64 s.Alignment <> 0L then
                push acc9 (finding s.Name FindingKind.BaseMisaligned (Text.append (Text.append "declared base " (Fmt.hex64 (uint64 baseValue))) (Text.append " is not a multiple of the declared alignment " (Fmt.ofInt s.Alignment))))
            else acc9
        if Availability.wellFormed s.Since s.Until then acc10
        else push acc10 (finding s.Name FindingKind.InvalidAvailability (Text.append (Text.append "available since " s.Since) (Text.append " until " s.Until)))

    let private checkProgramLifetime (acc: Finding array) (desc: PlatformDescription) : Finding array =
        let checkReference out role name immutable =
            let subject = Text.append "program-lifetime." role
            let named = checkName out subject name
            match PlatformDescription.tryFindSpace desc name with
            | None when name <> "" ->
                push named (finding subject FindingKind.UnknownSpace (Text.append "program-lifetime space is not declared: " name))
            | None -> named
            | Some space ->
                let permitted =
                    if immutable then space.Access = Access.ReadOnly || space.Access = Access.ReadExecute
                    else space.Access = Access.ReadWrite
                if permitted then named
                else push named (finding subject FindingKind.InvalidProgramLifetimeAccess
                                    (Text.append "program-lifetime role is incompatible with the declared access of " name))
        match desc.ProgramLifetime with
        | None -> acc
        | Some roles ->
            let immutable = checkReference acc "immutable" roles.Immutable true
            match roles.Mutable with
            | None -> immutable
            | Some name -> checkReference immutable "mutable" name false

    /// Two spaces with declared bases overlap when their ranges intersect.
    let private overlaps (a: MemorySpace) (b: MemorySpace) : bool =
        let ab =
            match a.Base with
            | Some v -> v
            | None -> 0L
        let bb =
            match b.Base with
            | Some v -> v
            | None -> 0L
        let aHas =
            match a.Base with
            | Some _ -> true
            | None -> false
        let bHas =
            match b.Base with
            | Some _ -> true
            | None -> false
        // Compare the later start with the earlier extent without overflowing
        // either an endpoint or the distance across zero. The SMT observer uses
        // mathematical integers; this observer must decide the same proposition.
        let startsWithin (start: int64) (capacity: int64) (later: int64) : bool =
            if start < 0L && later >= 0L then later < start + capacity
            else later - start < capacity
        aHas && bHas && a.Capacity > 0L && b.Capacity > 0L
        && (if ab <= bb then startsWithin ab a.Capacity bb else startsWithin bb b.Capacity ab)

    let private checkOverlaps (acc: Finding array) (spaces: MemorySpace array) : Finding array =
        let n = Array.length spaces
        let mutable out = acc
        let mutable i = 0
        while i < n do
            let a = Array.get spaces i
            let mutable j = i + 1
            while j < n do
                let b = Array.get spaces j
                if overlaps a b then
                    let subject = Text.append (Text.append a.Name "/") b.Name
                    out <- push out (finding subject FindingKind.OverlappingSpaces "spaces with declared bases overlap")
                j <- j + 1
            i <- i + 1
        out

    let private checkBuffer (acc: Finding array) (desc: PlatformDescription) (index: int) : Finding array =
        let b = Array.get desc.Buffers index
        let acc1 = checkName acc "buffer" b.Name
        let acc2 = checkDuplicate acc1 "buffer" b.Name (bufferSeenBefore desc.Buffers index b.Name)
        let acc3 = checkName acc2 (Text.append "buffer schema of " b.Name) b.Schema
        let acc4 = checkTag acc3 b.Name "framing" (Framing.isValid b.Framing) b.Framing
        let acc5 = checkTag acc4 b.Name "lifetime" (Lifetime.isValid b.Lifetime) b.Lifetime
        let acc6 = checkTag acc5 b.Name "access" (Access.isValid b.Access) b.Access
        let acc7 =
            if b.Capacity <= 0L then
                push acc6 (finding b.Name FindingKind.NonPositiveCapacity (Text.append "buffer capacity is not positive: " (Fmt.ofInt64 b.Capacity)))
            else acc6
        let space = PlatformDescription.spaceOfBuffer desc b
        let acc8 =
            match space with
            | None ->
                push acc7 (finding b.Name FindingKind.UnknownSpace (Text.append (Text.append "buffer names space '" b.Space) "', which is not declared"))
            | Some s ->
                if b.Capacity > s.Capacity then
                    let msg = Text.append (Text.append (Text.append "buffer capacity " (Fmt.ofInt64 b.Capacity)) " exceeds the capacity of its space: ") (Fmt.ofInt64 s.Capacity)
                    push acc7 (finding b.Name FindingKind.CapacityExceedsSpace msg)
                else acc7
        let acc9 =
            if Framing.hasDelimiter b.Framing && not (BufferSchema.hasDelimiter b) then
                push acc8 (finding b.Name FindingKind.MissingDelimiter (Text.append "delimited framing declares no delimiter byte (0..255): " (Fmt.ofInt b.Delimiter)))
            else acc8
        let ringOk = b.Framing <> Framing.Ring || (b.Slot > 0 && int64 b.Slot <= b.Capacity && b.Capacity % int64 b.Slot = 0L)
        if ringOk then acc9
        else push acc9 (finding b.Name FindingKind.InvalidSlot (Text.append "ring framing needs a positive slot that divides the capacity: " (Fmt.ofInt b.Slot)))

    let private checkEndpoint (acc: Finding array) (surface: BoundarySurface) (index: int) : Finding array =
        let e = Array.get surface.Endpoints index
        let subject = dotted surface.Name e.Name
        let acc1 = checkName acc (Text.append "endpoint of " surface.Name) e.Name
        let acc2 = checkDuplicate acc1 (Text.append "endpoint of " surface.Name) e.Name (endpointSeenBefore surface.Endpoints index e.Name)
        let acc3 = checkTag acc2 subject "location" (EndpointKind.isValid e.Location) e.Location
        let acc4 =
            if Availability.wellFormed e.Since e.Until then acc3
            else push acc3 (finding subject FindingKind.InvalidAvailability (Text.append (Text.append "available since " e.Since) (Text.append " until " e.Until)))
        checkContracts acc4 subject e.Contracts

    let private checkSurface (acc: Finding array) (surfaces: BoundarySurface array) (index: int) : Finding array =
        let s = Array.get surfaces index
        let acc1 = checkName acc "surface" s.Name
        let acc2 = checkDuplicate acc1 "surface" s.Name (surfaceSeenBefore surfaces index s.Name)
        let acc3 = checkTag acc2 s.Name "kind" (SurfaceKind.isValid s.Kind) s.Kind
        let n = Array.length s.Endpoints
        let mutable out = acc3
        let mutable i = 0
        while i < n do
            out <- checkEndpoint out s i
            i <- i + 1
        checkContracts out s.Name s.Contracts

    /// True when an endpoint with this name is declared on a surface before `upto`.
    let private endpointOnEarlierSurface (surfaces: BoundarySurface array) (upto: int) (name: string) : bool =
        let mutable i = 0
        let mutable seen = false
        while not seen && i < upto do
            seen <- indexOfEndpointIn (Array.get surfaces i).Endpoints name >= 0
            i <- i + 1
        seen

    let private checkEndpointNamesAcrossSurfaces (acc: Finding array) (surfaces: BoundarySurface array) : Finding array =
        let n = Array.length surfaces
        let mutable out = acc
        let mutable i = 1
        while i < n do
            let s = Array.get surfaces i
            let ne = Array.length s.Endpoints
            let mutable j = 0
            while j < ne do
                let e = Array.get s.Endpoints j
                if endpointOnEarlierSurface surfaces i e.Name then
                    out <- push out (finding (dotted s.Name e.Name) FindingKind.DuplicateName "endpoint name is declared on another surface too; transports resolve endpoints by name")
                j <- j + 1
            i <- i + 1
        out

    let private checkTransport (acc: Finding array) (desc: PlatformDescription) (index: int) : Finding array =
        let t = Array.get desc.Transports index
        let acc1 = checkName acc "transport" t.Name
        let acc2 = checkDuplicate acc1 "transport" t.Name (transportSeenBefore desc.Transports index t.Name)
        let acc3 = checkTag acc2 t.Name "kind" (TransportKind.isValid t.Kind) t.Kind
        let n = Array.length t.Endpoints
        let mutable out = acc3
        let mutable i = 0
        while i < n do
            let name = Array.get t.Endpoints i
            if not (PlatformDescription.hasEndpoint desc name) then
                out <- push out (finding t.Name FindingKind.UnknownEndpoint (Text.append (Text.append "transport names endpoint '" name) "', which no surface declares"))
            i <- i + 1
        let out2 =
            if t.RateHz < 0L || t.MaxUnit < 0L then push out (finding t.Name FindingKind.NegativeRate "transport rate and largest unit must be zero (undeclared) or positive")
            else out
        if Availability.wellFormed t.Since t.Until then out2
        else push out2 (finding t.Name FindingKind.InvalidAvailability (Text.append (Text.append "available since " t.Since) (Text.append " until " t.Until)))

    let private limitSeenBefore (limits: Limit array) (upto: int) (name: string) : bool =
        let mutable i = 0
        let mutable seen = false
        while not seen && i < upto do
            seen <- (Array.get limits i).Name = name
            i <- i + 1
        seen

    let private checkLimit (acc: Finding array) (limits: Limit array) (index: int) : Finding array =
        let l = Array.get limits index
        let acc1 = checkName acc "limit" l.Name
        let acc2 = checkDuplicate acc1 "limit" l.Name (limitSeenBefore limits index l.Name)
        if l.Value < 0L then push acc2 (finding l.Name FindingKind.NegativeLimit (Text.append "limit is negative: " (Fmt.ofInt64 l.Value)))
        else acc2

    let private checkLifecycle (acc: Finding array) (l: LifecycleFacts) : Finding array =
        let acc1 = checkTag acc "lifecycle" "persistence" (Persistence.isValid l.Persistence) l.Persistence
        let nc = Array.length l.Clocks
        let mutable out = acc1
        let mutable i = 0
        while i < nc do
            let c = Array.get l.Clocks i
            out <- checkName out "clock" c.Name
            if c.FrequencyHz <= 0L then
                out <- push out (finding c.Name FindingKind.NonPositiveFrequency (Text.append "clock frequency is not positive: " (Fmt.ofInt64 c.FrequencyHz)))
            i <- i + 1
        let nr = Array.length l.Resets
        let mutable j = 0
        while j < nr do
            let r = Array.get l.Resets j
            out <- checkName out "reset" r.Name
            j <- j + 1
        out

    let private widthSeenBefore (widths: WidthDeclaration array) (upto: int) (name: string) : bool =
        let mutable seen = false
        let mutable j = 0
        while not seen && j < upto do
            seen <- (Array.get widths j).Name = name
            j <- j + 1
        seen

    let private representationSeenBefore (representations: Representation array) (upto: int) (name: string) : bool =
        let mutable seen = false
        let mutable j = 0
        while not seen && j < upto do
            seen <- (Array.get representations j).Name = name
            j <- j + 1
        seen

    /// A width dimension: a name declared once, at a positive number of bits.
    let private checkWidth (acc: Finding array) (widths: WidthDeclaration array) (index: int) : Finding array =
        let w = Array.get widths index
        let acc1 = checkName acc "width" w.Name
        let acc2 = checkDuplicate acc1 "width" w.Name (widthSeenBefore widths index w.Name)
        if w.Bits > 0 then acc2
        else push acc2 (finding w.Name FindingKind.NonPositiveWidth (Text.append "width in bits is not positive: " (Fmt.ofInt w.Bits)))

    /// A representation: a name declared once, tags the vocabulary names, a
    /// positive number of bits, and both range bounds stated.
    let private checkRepresentation (acc: Finding array) (representations: Representation array) (index: int) : Finding array =
        let r = Array.get representations index
        let acc1 = checkName acc "representation" r.Name
        let acc2 = checkDuplicate acc1 "representation" r.Name (representationSeenBefore representations index r.Name)
        let acc3 = checkTag acc2 r.Name "capability" (Capability.isValid r.Capability) r.Capability
        let acc4 = checkTag acc3 r.Name "family" (RepresentationFamily.isValid r.Family) r.Family
        let acc5 = checkTag acc4 r.Name "boundary" (Boundary.isValid r.Boundary) r.Boundary
        let acc6 =
            if r.Bits > 0 then acc5
            else push acc5 (finding r.Name FindingKind.NonPositiveWidth (Text.append "representation width in bits is not positive: " (Fmt.ofInt r.Bits)))
        if String.length r.MinMagnitude = 0 || String.length r.MaxMagnitude = 0 then
            push acc6 (finding r.Name FindingKind.MissingRange "representation declares no range bounds (MinMagnitude and MaxMagnitude as decimal text)")
        else
            let problem =
                match DecimalText.compare r.MinMagnitude r.MaxMagnitude with
                | None -> "range bounds must be finite plain decimal text (optional sign, digits, optional fractional part)"
                | Some order when order > 0 -> "MinMagnitude exceeds MaxMagnitude"
                | Some _ ->
                    if (r.Family = RepresentationFamily.Int || r.Family = RepresentationFamily.UInt)
                       && not (DecimalText.isInteger r.MinMagnitude && DecimalText.isInteger r.MaxMagnitude) then
                        "integer representation bounds must be integer decimal text"
                    elif r.Family = RepresentationFamily.UInt && DecimalText.compare r.MinMagnitude "0" = Some -1 then
                        "unsigned representation range includes negative values"
                    else ""
            if String.length problem = 0 then acc6
            else push acc6 (finding r.Name FindingKind.InvalidRange problem)

    /// The declared Register width, or 0 when the core declares none.
    let private registerBits (widths: WidthDeclaration array) : int =
        let n = Array.length widths
        let mutable i = 0
        let mutable bits = 0
        while bits = 0 && i < n do
            let w = Array.get widths i
            if w.Name = WidthDeclaration.Register then bits <- w.Bits
            i <- i + 1
        bits

    let private checkCore (acc: Finding array) (c: TargetCore) : Finding array =
        let acc1 = checkName acc "core os" c.Os
        let acc2 = checkName acc1 "core arch" c.Arch
        let endianOk = c.Endianness = "little" || c.Endianness = "big"
        let acc3 = checkTag acc2 "core" "endianness" endianOk c.Endianness
        let acc4 =
            if isPowerOfTwo c.WordSizeBits then acc3
            else push acc3 (finding "core" FindingKind.AlignmentNotPowerOfTwo (Text.append "word size in bits is not a power of two: " (Fmt.ofInt c.WordSizeBits)))
        let nw = Array.length c.Widths
        let mutable out = acc4
        let mutable i = 0
        while i < nw do
            out <- checkWidth out c.Widths i
            i <- i + 1
        let nr = Array.length c.Representations
        let mutable j = 0
        while j < nr do
            out <- checkRepresentation out c.Representations j
            j <- j + 1
        // The word size and a declared Register width state one fact; they must agree.
        let reg = registerBits c.Widths
        if reg = 0 || reg = c.WordSizeBits then out
        else push out (finding "core" FindingKind.WidthMismatch (Text.append (Text.append "declared Register width " (Fmt.ofInt reg)) (Text.append " disagrees with the word size " (Fmt.ofInt c.WordSizeBits))))

    /// Every finding about the description, in declaration order; empty when
    /// the description is consistent.
    let run (desc: PlatformDescription) : Finding array =
        let start : Finding array = Array.zeroCreate 0
        let acc0 = checkName start "description" desc.Id
        let acc1 =
            match desc.Core with
            | Some c -> checkCore acc0 c
            | None -> acc0
        let ns = Array.length desc.Spaces
        let mutable acc = acc1
        let mutable i = 0
        while i < ns do
            acc <- checkSpace acc desc.Spaces i
            i <- i + 1
        acc <- checkOverlaps acc desc.Spaces
        acc <- checkProgramLifetime acc desc
        let nb = Array.length desc.Buffers
        let mutable j = 0
        while j < nb do
            acc <- checkBuffer acc desc j
            j <- j + 1
        let nsf = Array.length desc.Surfaces
        let mutable k = 0
        while k < nsf do
            acc <- checkSurface acc desc.Surfaces k
            k <- k + 1
        acc <- checkEndpointNamesAcrossSurfaces acc desc.Surfaces
        let nt = Array.length desc.Transports
        let mutable m = 0
        while m < nt do
            acc <- checkTransport acc desc m
            m <- m + 1
        let nl = Array.length desc.Limits
        let mutable q = 0
        while q < nl do
            acc <- checkLimit acc desc.Limits q
            q <- q + 1
        checkLifecycle acc desc.Lifecycle

    /// True when a buffer's space is one the far side reads directly: a
    /// kernel map, a ring, or a view over a host buffer.
    let private sharedSpace (desc: PlatformDescription) (b: BufferSchema) : bool =
        let s = PlatformDescription.spaceOfBuffer desc b
        let kind =
            match s with
            | Some sp -> sp.Kind
            | None -> ""
        kind = MemoryKind.Map || kind = MemoryKind.Ring || kind = MemoryKind.ViewBacked

    /// `run`, plus the pointer-free rule for every buffer on a shared space
    /// whose schema resolves to a layout: a layout with a Pointer
    /// representation is a `PointerInSharedLayout` finding (docs/11, "Map
    /// values are pointer-free by construction"). `layoutOf` maps a schema
    /// name to its descriptor layout, or None when the schema is not a
    /// descriptor.
    let runWithLayouts (desc: PlatformDescription) (layoutOf: string -> BAREWire.Hardware.PeripheralLayout option) : Finding array =
        let mutable acc = run desc
        let nb = Array.length desc.Buffers
        let mutable i = 0
        while i < nb do
            let b = Array.get desc.Buffers i
            let layout = layoutOf b.Schema
            let pointerFree =
                match layout with
                | Some l -> BAREWire.Hardware.Layout.isPointerFree l
                | None -> true
            if sharedSpace desc b && not pointerFree then
                acc <- push acc (finding b.Name FindingKind.PointerInSharedLayout (Text.append (Text.append "buffer on shared space " b.Space) " has a layout with a pointer field"))
            i <- i + 1
        acc

    let private isSpaceChar (c: char) : bool =
        System.Char.IsWhiteSpace c

    let private isBlankText (s: string) : bool =
        let n = String.length s
        let mutable i = 0
        while i < n && isSpaceChar s.[i] do
            i <- i + 1
        i = n

    let private checkRequiredText (acc: Finding array) (subject: string) (field: string) (value: string) : Finding array =
        if isBlankText value then
            push acc (finding subject FindingKind.EmptyName (Text.append field " is empty or whitespace"))
        else acc

    let private endsWithText (s: string) (suffix: string) : bool =
        let sn = String.length s
        let pn = String.length suffix
        if pn > sn then false
        else
            let offset = sn - pn
            let mutable i = 0
            let mutable ok = true
            while ok && i < pn do
                ok <- s.[offset + i] = suffix.[i]
                i <- i + 1
            ok

    let private basenameStart (s: string) : int =
        let n = String.length s
        let mutable i = n - 1
        while i >= 0 && s.[i] <> '/' && s.[i] <> '\\' do
            i <- i - 1
        i + 1

    let private isLinuxSharedObjectName (path: string) : bool =
        let start = basenameStart path
        let n = String.length path
        let nameLen = n - start
        if nameLen <= 3 then false
        elif endsWithText path ".so" then start <= n - 4
        else
            let mutable i = start + 1
            let mutable markerAt = -1
            while i + 4 <= n do
                if path.[i] = '.' && path.[i + 1] = 's' && path.[i + 2] = 'o' && path.[i + 3] = '.' then
                    markerAt <- i
                i <- i + 1
            if markerAt < 0 then false
            else
                let mutable pos = markerAt + 4
                let mutable ok = pos < n
                let mutable segDigits = 0
                while ok && pos < n do
                    let c = path.[pos]
                    if c >= '0' && c <= '9' then
                        segDigits <- segDigits + 1
                    elif c = '.' && segDigits > 0 then
                        segDigits <- 0
                    else
                        ok <- false
                    pos <- pos + 1
                ok && segDigits > 0

    let private isHex64 (s: string) : bool =
        if String.length s <> 64 then false
        else
            let mutable i = 0
            let mutable ok = true
            while ok && i < 64 do
                let c = s.[i]
                ok <- (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
                i <- i + 1
            ok

    /// Validate a declared static archive contract's path, provenance, digest,
    /// member payload format, and required resulting symbol visibility.
    let checkStaticArchiveLink (subject: string) (archive: StaticArchiveContract) : Finding array =
        let label = if isBlankText subject then "static-archive" else subject
        let mutable acc = Array.zeroCreate 0
        acc <- checkRequiredText acc label "ArchivePath" archive.ArchivePath
        if not (isBlankText archive.ArchivePath)
           && (not (endsWithText archive.ArchivePath ".a") || basenameStart archive.ArchivePath > String.length archive.ArchivePath - 3) then
            acc <- push acc (finding label FindingKind.InvalidLinkArtifact (Text.append "static archive path must name a .a archive container: " archive.ArchivePath))
        acc <- checkRequiredText acc label "ArchiveSource" archive.ArchiveSource
        if isBlankText archive.Sha256 then
            acc <- push acc (finding label FindingKind.EmptyName "Sha256 is empty or whitespace")
        elif not (isHex64 archive.Sha256) then
            acc <- push acc (finding label FindingKind.InvalidArchiveDigest "Sha256 must be exactly 64 ASCII hexadecimal characters")
        acc <- checkTag acc label "archive-format" (ArchiveFormat.isValid archive.Format) archive.Format
        checkTag acc label "symbol-visibility" (SymbolVisibility.isValid archive.Visibility) archive.Visibility

    /// Validate a dynamic shared-object or static archive `ProcessLinkSpec` without
    /// altering `[<FidelityExtern>]` declarations (`Static Binding Elision Test`).
    let checkProcessLink (spec: ProcessLinkSpec) : Finding array =
        let c = spec.Contract
        let subject = if isBlankText c.Identity then "process-link" else c.Identity
        let mutable acc = Array.zeroCreate 0
        acc <- checkRequiredText acc subject "Identity" c.Identity
        acc <- checkRequiredText acc subject "Version" c.Version
        acc <- checkRequiredText acc subject "Source" c.Source
        acc <- checkRequiredText acc subject "Library" c.Library
        acc <- checkRequiredText acc subject "Runtime" c.Runtime
        acc <- checkRequiredText acc subject "Os" c.Os
        acc <- checkRequiredText acc subject "Arch" c.Arch
        acc <- checkRequiredText acc subject "LoadAt" c.LoadAt
        acc <- checkRequiredText acc subject "LiveUntil" c.LiveUntil
        acc <- checkRequiredText acc subject "Unloading" c.Unloading
        acc <- checkRequiredText acc subject "ExportedLibrary" c.ExportedLibrary
        acc <- checkTag acc subject "link-kind" (LinkKind.isValid spec.LinkKind) spec.LinkKind
        if not (isBlankText c.Unloading) && c.Unloading <> "Forbidden" then
            acc <- push acc (finding subject FindingKind.InvalidLinkPolicy "process-link Unloading must be Forbidden")
        if not (isBlankText c.ExportedLibrary) && c.ExportedLibrary <> "Forbidden" then
            acc <- push acc (finding subject FindingKind.InvalidLinkPolicy "process-link ExportedLibrary must be Forbidden")
        if not (isBlankText c.LiveUntil) && c.LiveUntil <> "ProcessTermination" then
            acc <- push acc (finding subject FindingKind.InvalidLinkPolicy "process-link LiveUntil must be ProcessTermination")
        if spec.LinkKind = LinkKind.DynamicSharedObject then
            if not (isBlankText c.LoadAt) && c.LoadAt <> "ProcessStartup" then
                acc <- push acc (finding subject FindingKind.InvalidLinkPolicy "dynamic process-link LoadAt must be ProcessStartup")
            match spec.StaticArchive with
            | Some _ ->
                acc <- push acc (finding subject FindingKind.InvalidLinkArtifact "dynamic shared-object link contract must not declare a static archive")
            | None -> ()
            if spec.DynamicSharedObject <> "" then
                let soOk =
                    if isBlankText spec.DynamicSharedObject then false
                    elif c.Os = "linux" then isLinuxSharedObjectName spec.DynamicSharedObject
                    elif c.Os = "windows" then endsWithText spec.DynamicSharedObject ".dll" && basenameStart spec.DynamicSharedObject <= String.length spec.DynamicSharedObject - 5
                    else false
                if not soOk then
                    acc <- push acc (finding subject FindingKind.InvalidLinkArtifact (Text.append "dynamic shared-object artifact does not match OS naming policy: " spec.DynamicSharedObject))
        elif spec.LinkKind = LinkKind.StaticArchive then
            if not (isBlankText c.LoadAt) && c.LoadAt <> "ProcessStartup" && c.LoadAt <> "StaticLink" then
                acc <- push acc (finding subject FindingKind.InvalidLinkPolicy "static archive LoadAt must be ProcessStartup or StaticLink")
            if spec.DynamicSharedObject <> "" then
                acc <- push acc (finding subject FindingKind.InvalidLinkArtifact "static archive link contract must not declare DynamicSharedObject")
            match spec.StaticArchive with
            | None ->
                acc <- push acc (finding subject FindingKind.InvalidLinkArtifact "static archive link contract requires StaticArchive")
            | Some archive ->
                let archiveFindings = checkStaticArchiveLink subject archive
                let na = Array.length archiveFindings
                let mutable ai = 0
                while ai < na do
                    acc <- push acc (Array.get archiveFindings ai)
                    ai <- ai + 1
        acc

    /// Check one cross-call placement without changing the platform record.
    let checkSharedBufferPlacement (desc: PlatformDescription) (p: SharedBufferPlacement) : Finding array =
        let s = p.SpaceDescriptor
        let layout = p.StructLayout.Layout
        let space = PlatformDescription.tryFindSpace desc s.Space
        let buffer = PlatformDescription.tryFindSharedBufferSchema desc p
        let orderMatches =
            SharedBufferPlacement.validByteOrder s.ByteOrder
            && SharedBufferPlacement.validByteOrder p.LayoutByteOrder
            && s.ByteOrder = p.LayoutByteOrder
            && (match desc.Core with
                | Some core -> SharedBufferPlacement.validByteOrder core.Endianness && s.ByteOrder = core.Endianness
                | None -> false)
        let alignmentValid =
            SharedBufferPlacement.isPowerOfTwo p.LayoutAlignment
            && SharedBufferPlacement.isPowerOfTwo s.RegionAlignment
            && layout.Alignment = p.LayoutAlignment
            && p.MeasuredElementSize % p.LayoutAlignment = 0
            && s.RegionAlignment >= p.LayoutAlignment
            && s.RegionAlignment % p.LayoutAlignment = 0
            && p.SpaceOffset >= 0L
            && p.SpaceOffset % int64 s.RegionAlignment = 0L
        let bufferSchemaMatched =
            Array.length desc.Buffers = 0 || Option.isSome buffer
        let layoutValid =
            SharedBufferPlacement.trimmedLength p.Name > 0
            && not p.HasRawPointerFields
            && not (SharedBufferPlacement.structHasRawPointerFields p.StructLayout)
            && not (SharedBufferPlacement.structHasFlexibleArray p.StructLayout)
            && p.HasVerifiedExports && p.MeasuredElementSize > 0 && layout.Size > 0
            && layout.Size = p.MeasuredElementSize && s.ElementSize = p.MeasuredElementSize
            && alignmentValid && orderMatches && SharedBufferPlacement.viewLayoutValid p
            && SharedBufferPlacement.structFieldsGeometryValid p
            && bufferSchemaMatched
            && (match space with
                | Some sp ->
                    SharedBufferPlacement.spaceKindMatches sp.Kind s.MemoryRegion
                    && SharedBufferPlacement.spaceAlignmentValid sp.Alignment s.RegionAlignment
                | None -> false)
        let requiredBytes = int64 s.ElementCount * int64 p.MeasuredElementSize
        let bufferCapacityOk =
            match buffer with
            | Some b -> b.Capacity > 0L && int64 s.RegionByteExtent <= b.Capacity
            | None -> Array.length desc.Buffers = 0 && Option.isSome space
        let extentValid =
            s.ElementSize > 0 && p.MeasuredElementSize > 0 && s.ElementSize = p.MeasuredElementSize
            && s.ElementCount > 0 && s.RegionByteExtent > 0
            && requiredBytes <= int64 System.Int32.MaxValue
            && int64 s.RegionByteExtent >= requiredBytes
            && p.SpaceOffset >= 0L
            && bufferCapacityOk
            && (match space with
                | Some sp -> sp.Capacity > 0L
                             && int64 s.RegionByteExtent <= sp.Capacity
                             && p.SpaceOffset <= sp.Capacity - int64 s.RegionByteExtent
                | None -> false)
            && p.MaxAccessedElementIndex >= 0 && p.MaxAccessedElementIndex < s.ElementCount
        let lifetimeValid =
            s.RegionLifetimeDepth > 0 && s.LibraryRetentionDepth >= 0
            && s.RegionLifetimeDepth <= s.LibraryRetentionDepth
            && p.MappedDepth >= 0 && p.MappedDepth <= p.FirstUseDepth
            && p.FirstUseDepth <= p.LastUseDepth
            && p.LastUseDepth <= p.LibraryRetentionEndDepth && p.LibraryRetentionEndDepth < p.UnmapDepth
            && not p.LeakedAtExit
        let bufferAccessOk =
            match buffer with
            | Some b -> SharedBufferPlacement.spaceAccessMatches b.Access s.Access
            | None -> Array.length desc.Buffers = 0 && Option.isSome space
        let accessValid =
            SharedBufferPlacement.validAccess s.Access
            && SharedBufferPlacement.viewAccessMatches p
            && SharedBufferPlacement.structFieldAccessMatches p
            && bufferAccessOk
            && (match space with
                | Some sp -> SharedBufferPlacement.spaceAccessMatches sp.Access s.Access
                | None -> false)
            && not (s.Access = "ReadOnly" && p.LibraryMutatesBuffer)
            && not p.ConcurrentClefMutation && not p.OverlappingMutableShare
        let mutable acc = [||]
        if not layoutValid then
            acc <- push acc (finding p.Name FindingKind.SharedBufferLayoutInvalid "FS9220: shared-buffer layout, alignment, byte order, exports or space is invalid")
        if not extentValid then
            acc <- push acc (finding p.Name FindingKind.SharedBufferExtentInvalid "FS9221: shared-buffer extent or accessed element is outside its bounds")
        if not lifetimeValid then
            acc <- push acc (finding p.Name FindingKind.SharedBufferLifetimeInvalid "FS9222: shared-buffer mapping does not cover its uses and library retention")
        if not accessValid then
            acc <- push acc (finding p.Name FindingKind.SharedBufferConflictingAccess "FS9223: shared-buffer access conflicts with the library's view")
        acc

    let private nonEmptyRoleSymbols (r: BAREWire.Descriptors.SharedBufferRegionDescriptor) : string array =
        let acq = SharedBufferPlacement.trimmedText r.Acquire
        let rel = match r.Release with Some v -> SharedBufferPlacement.trimmedText v | None -> ""
        let syncs = r.Sync |> Array.map SharedBufferPlacement.trimmedText
        let foreigns = r.ForeignUses |> Array.map SharedBufferPlacement.trimmedText
        Array.concat [| [| acq; rel |]; syncs; foreigns |]
        |> Array.filter (fun s -> s.Length > 0)

    /// Validate a declared SharedBufferRegionDescriptor against the placement table.
    let checkSharedBufferRegion (placements: SharedBufferPlacement array) (region: BAREWire.Descriptors.SharedBufferRegionDescriptor) : Finding array =
        let placementName = SharedBufferPlacement.trimmedText region.Placement
        let subject = if placementName.Length > 0 then placementName else "shared-buffer-region"
        let matched = placements |> Array.tryFind (fun p -> SharedBufferPlacement.trimmedText p.Name = placementName)
        let acq = SharedBufferPlacement.trimmedText region.Acquire
        let relOpt = region.Release |> Option.map SharedBufferPlacement.trimmedText
        let syncTrimmed = region.Sync |> Array.map SharedBufferPlacement.trimmedText
        let foreignTrimmed = region.ForeignUses |> Array.map SharedBufferPlacement.trimmedText
        let acquireOk = acq.Length > 0
        let releaseOk =
            match matched, relOpt with
            | Some p, Some rel -> not p.LeakedAtExit && rel.Length > 0 && rel <> acq
            | Some p, None -> p.LeakedAtExit
            | None, Some rel -> rel.Length > 0 && rel <> acq
            | None, None -> false
        let mutable syncOk = true
        for si = 0 to syncTrimmed.Length - 1 do
            let s = syncTrimmed.[si]
            let mutable dup = false
            for sj = 0 to si - 1 do
                if syncTrimmed.[sj] = s then dup <- true
            if s.Length = 0 || s = acq || dup || (match relOpt with Some rel -> s = rel | None -> false) then
                syncOk <- false
        let mutable foreignOk = true
        for ui = 0 to foreignTrimmed.Length - 1 do
            let u = foreignTrimmed.[ui]
            let mutable dup = false
            for uj = 0 to ui - 1 do
                if foreignTrimmed.[uj] = u then dup <- true
            if u.Length = 0 || u = acq || dup || Array.contains u syncTrimmed || (match relOpt with Some rel -> u = rel | None -> false) then
                foreignOk <- false
        let mutable acc = [||]
        if Option.isNone matched then
            acc <- push acc (finding subject FindingKind.SharedBufferLayoutInvalid "FS9220: shared-buffer region descriptor names an undeclared placement")
        if not (acquireOk && releaseOk && syncOk && foreignOk) then
            acc <- push acc (finding subject FindingKind.SharedBufferLifetimeInvalid "FS9222: shared-buffer region descriptor call-role bindings are incomplete or collide")
        acc

    /// Validate a table of SharedBufferRegionDescriptors against the placement table.
    let checkSharedBufferRegions (placements: SharedBufferPlacement array) (regions: BAREWire.Descriptors.SharedBufferRegionDescriptor array) : Finding array =
        let mutable acc = [||]
        for p in placements do
            let pName = SharedBufferPlacement.trimmedText p.Name
            let hasRegion =
                regions |> Array.exists (fun r -> SharedBufferPlacement.trimmedText r.Placement = pName)
            if not hasRegion then
                acc <- push acc (finding p.Name FindingKind.SharedBufferLifetimeInvalid "FS9222: shared-buffer placement has no region call-role descriptor")
        for i = 0 to regions.Length - 1 do
            let r = regions.[i]
            let rPlacement = SharedBufferPlacement.trimmedText r.Placement
            let subject = if rPlacement.Length > 0 then rPlacement else "shared-buffer-region"
            let findings = checkSharedBufferRegion placements r
            for f in findings do
                acc <- push acc f
            let rSyms = nonEmptyRoleSymbols r
            let mutable dupPlacement = false
            let mutable sharedRoleSymbol = false
            let mutable j = 0
            while j < i do
                let prior = regions.[j]
                let priorPlacement = SharedBufferPlacement.trimmedText prior.Placement
                if priorPlacement = rPlacement && rPlacement.Length > 0 then
                    dupPlacement <- true
                else
                    let priorSyms = nonEmptyRoleSymbols prior
                    if rSyms |> Array.exists (fun sym -> Array.contains sym priorSyms) then
                        sharedRoleSymbol <- true
                j <- j + 1
            if dupPlacement then
                acc <- push acc (finding subject FindingKind.SharedBufferLifetimeInvalid "FS9222: shared-buffer region descriptor placement is bound more than once")
            if sharedRoleSymbol then
                acc <- push acc (finding subject FindingKind.SharedBufferLayoutInvalid "FS9220: shared-buffer region descriptor call-role binding is shared across placements")
        acc

    /// Findings in placement and rule order; repeated names also fail FS9220.
    let checkSharedBuffers (desc: PlatformDescription) (placements: SharedBufferPlacement array) : Finding array =
        let mutable acc = [||]
        for i = 0 to placements.Length - 1 do
            let p = placements.[i]
            let findings = checkSharedBufferPlacement desc p
            for f in findings do
                acc <- push acc f
            if SharedBufferPlacement.nameSeenBefore placements i then
                acc <- push acc (finding p.Name FindingKind.SharedBufferLayoutInvalid "FS9220: shared-buffer placement name is declared more than once")
        for sp in desc.Spaces do
            let placed = placements |> Array.filter (fun p -> p.SpaceDescriptor.Space = sp.Name)
            if placed.Length >= 2 then
                let total = placed |> Array.sumBy (fun p -> int64 p.SpaceDescriptor.RegionByteExtent)
                if total > sp.Capacity then
                    acc <- push acc (finding sp.Name FindingKind.SharedBufferExtentInvalid "FS9221: aggregate shared-buffer extent exceeds space capacity")
                let endsAfter (p: SharedBufferPlacement) offset =
                    let length = int64 p.SpaceDescriptor.RegionByteExtent
                    length > 0L && (p.SpaceOffset > System.Int64.MaxValue - length || offset < p.SpaceOffset + length)
                for i = 0 to placed.Length - 2 do
                    for j = i + 1 to placed.Length - 1 do
                        let a, b = placed.[i], placed.[j]
                        if endsAfter b a.SpaceOffset && endsAfter a b.SpaceOffset then
                            acc <- push acc (finding sp.Name FindingKind.SharedBufferExtentInvalid "FS9221: shared-buffer placements overlap within the same memory space")
        acc

    /// One line per finding: `<kind> <subject>: <message>`; "no findings"
    /// when there are none.
    let explain (findings: Finding array) : string =
        let n = Array.length findings
        if n = 0 then "no findings"
        else
            let lines : string array = Array.zeroCreate n
            let mutable i = 0
            while i < n do
                let f = Array.get findings i
                let line = Text.append (Text.append (Text.append (Text.append f.Kind " ") f.Subject) ": ") f.Message
                Array.set lines i line
                i <- i + 1
            Fmt.join "\n" lines

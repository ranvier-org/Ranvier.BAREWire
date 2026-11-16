namespace BAREWire.Platform

open BAREWire.Encoding

/// A measured struct view placed in a platform space, retained across calls.
/// StructLayout.Layout owns the struct's size, alignment and field geometry.
/// Lexical scope depth axis on SpaceDescriptor (deeper is shorter lived):
/// 0 < RegionLifetimeDepth <= LibraryRetentionDepth.
/// Execution-sequence step axis on SharedBufferPlacement:
/// 0 <= MappedDepth <= FirstUseDepth <= LastUseDepth <= LibraryRetentionEndDepth < UnmapDepth.
/// Unmap occurs strictly after all uses and after library retention ends.
/// Export verification is evidence supplied by the binding producer.
type SharedBufferPlacement = {
    Name: string
    SpaceDescriptor: BAREWire.Descriptors.MemorySpaceDescriptor
    SpaceOffset: int64
    StructLayout: BAREWire.Hardware.StructDescriptor
    ViewLayout: BAREWire.Descriptors.ViewLayoutDescriptor
    MeasuredElementSize: int
    LayoutAlignment: int
    LayoutByteOrder: string
    HasRawPointerFields: bool
    HasVerifiedExports: bool
    MappedDepth: int
    FirstUseDepth: int
    LastUseDepth: int
    LibraryRetentionEndDepth: int
    UnmapDepth: int
    LeakedAtExit: bool
    ConcurrentClefMutation: bool
    LibraryMutatesBuffer: bool
    OverlappingMutableShare: bool
    MaxAccessedElementIndex: int
}

/// Shared evidence helpers; no record-literal defaults imply verification.
module SharedBufferPlacement =

    let internal isSpaceChar (c: char) : bool =
        c = ' ' || c = '\t' || c = '\r' || c = '\n'

    /// Substring of `s` with leading and trailing whitespace removed.
    let trimmedText (s: string) : string =
        let n = String.length s
        let mutable lo = 0
        while lo < n && isSpaceChar s.[lo] do
            lo <- lo + 1
        let mutable hi = n - 1
        while hi >= lo && isSpaceChar s.[hi] do
            hi <- hi - 1
        if hi < lo then "" else s.Substring(lo, hi - lo + 1)

    /// Non-whitespace length of `s` (0 when empty or all whitespace).
    let trimmedLength (s: string) : int =
        let n = String.length s
        let mutable lo = 0
        while lo < n && isSpaceChar s.[lo] do
            lo <- lo + 1
        let mutable hi = n - 1
        while hi >= lo && isSpaceChar s.[hi] do
            hi <- hi - 1
        if hi < lo then 0 else hi - lo + 1

    /// Include both fixed fields and any trailing flexible member.
    let structHasRawPointerFields (layout: BAREWire.Hardware.StructDescriptor) : bool =
        not (BAREWire.Hardware.Layout.isPointerFree layout.Layout)
        || (match layout.FlexibleArray with
            | Some field -> field.Repr = BAREWire.Hardware.Repr.Pointer
            | None -> false)

    /// Fixed-stride shared buffers forbid trailing flexible array members of any element type.
    let structHasFlexibleArray (layout: BAREWire.Hardware.StructDescriptor) : bool =
        match layout.FlexibleArray with
        | Some _ -> true
        | None -> false

    /// Count of raw pointer sources on a placement (HasRawPointerFields flag, pointer fields, pointer flex member).
    let rawPointerFieldCount (p: SharedBufferPlacement) : int =
        let flagCount = if p.HasRawPointerFields then 1 else 0
        let fields = p.StructLayout.Layout.Fields
        let mutable fieldPtrs = 0
        let mutable i = 0
        while i < fields.Length do
            if fields.[i].Repr = BAREWire.Hardware.Repr.Pointer then
                fieldPtrs <- fieldPtrs + 1
            i <- i + 1
        let flexPtr =
            match p.StructLayout.FlexibleArray with
            | Some field when field.Repr = BAREWire.Hardware.Repr.Pointer -> 1
            | _ -> 0
        flagCount + fieldPtrs + flexPtr

    let isPowerOfTwo (value: int) : bool =
        value > 0 && (value &&& (value - 1)) = 0

    let validByteOrder (order: string) : bool =
        order = "little" || order = "big"

    /// Integer encoding for byte order: 1 = little, 2 = big, 0 = invalid/empty.
    let byteOrderCode (order: string) : int64 =
        if order = "little" then 1L
        elif order = "big" then 2L
        else 0L

    let validAccess (access: string) : bool =
        access = "ReadOnly" || access = "WriteOnly"
        || access = "ReadWrite" || access = "ExclusiveMutable"

    /// Integer encoding for SpaceDescriptor.Access: 1 = ReadOnly, 2 = WriteOnly, 3 = ReadWrite, 4 = ExclusiveMutable, 0 = invalid.
    let declaredAccessCode (access: string) : int64 =
        if access = "ReadOnly" then 1L
        elif access = "WriteOnly" then 2L
        elif access = "ReadWrite" then 3L
        elif access = "ExclusiveMutable" then 4L
        else 0L

    /// Integer encoding for Hardware.AccessKind: 1 = ro, 2 = wo, 3 = rw, 0 = invalid.
    let hardwareAccessCode (access: BAREWire.Hardware.AccessKind) : int64 =
        if access = BAREWire.Hardware.AccessKind.ReadOnly then 1L
        elif access = BAREWire.Hardware.AccessKind.WriteOnly then 2L
        elif access = BAREWire.Hardware.AccessKind.ReadWrite then 3L
        else 0L

    /// Integer encoding for Platform.Access: 1 = r, 2 = w, 3 = rw, 0 = other/invalid.
    let platformAccessCode (access: Access) : int64 =
        if access = Access.ReadOnly then 1L
        elif access = Access.WriteOnly then 2L
        elif access = Access.ReadWrite then 3L
        else 0L

    /// Integer encoding for MemoryKind on shared spaces: 1 = sram, 2 = dma, 3 = flash, 4 = peripheral, 5 = view-backed, 0 = other.
    let memoryKindCode (kind: MemoryKind) : int64 =
        if kind = MemoryKind.Sram then 1L
        elif kind = MemoryKind.Dma then 2L
        elif kind = MemoryKind.Flash then 3L
        elif kind = MemoryKind.Peripheral then 4L
        elif kind = MemoryKind.ViewBacked then 5L
        else 0L

    /// Integer encoding for Hardware.MemoryRegionKind: 1 = sram, 2 = dma, 3 = flash, 4 = peripheral, 0 = other.
    let memoryRegionCode (region: BAREWire.Hardware.MemoryRegionKind) : int64 =
        if region = BAREWire.Hardware.MemoryRegionKind.SRAM then 1L
        elif region = BAREWire.Hardware.MemoryRegionKind.DMA then 2L
        elif region = BAREWire.Hardware.MemoryRegionKind.Flash then 3L
        elif region = BAREWire.Hardware.MemoryRegionKind.Peripheral then 4L
        else 0L

    let viewElementWidth (repr: BAREWire.Hardware.Repr) : int =
        match repr with
        | BAREWire.Hardware.Repr.U8 | BAREWire.Hardware.Repr.I8 | BAREWire.Hardware.Repr.Bool -> 1
        | BAREWire.Hardware.Repr.U16 | BAREWire.Hardware.Repr.I16 -> 2
        | BAREWire.Hardware.Repr.U32 | BAREWire.Hardware.Repr.I32 | BAREWire.Hardware.Repr.F32 -> 4
        | BAREWire.Hardware.Repr.U64 | BAREWire.Hardware.Repr.I64 | BAREWire.Hardware.Repr.F64 -> 8
        | _ -> 0

    let viewElementFits (p: SharedBufferPlacement) : bool =
        let width = viewElementWidth p.ViewLayout.Element
        width > 0 && p.MeasuredElementSize >= width && p.MeasuredElementSize % width = 0
        && p.LayoutAlignment > 0 && p.MeasuredElementSize % p.LayoutAlignment = 0
        && p.ViewLayout.Alignment >= width && p.LayoutAlignment >= width
        && p.ViewLayout.Alignment % width = 0

    /// Count of prior fields in `fields.[0 .. index - 1]` with the same name as `fields.[index]`.
    let priorFieldNameCount (fields: BAREWire.Hardware.FieldDescriptor array) (index: int) : int =
        let name = fields.[index].Name
        let mutable count = 0
        let mutable j = 0
        while j < index do
            if fields.[j].Name = name then count <- count + 1
            j <- j + 1
        count

    /// Count of prior bitfields in `bfs.[0 .. index - 1]` with the same name as `bfs.[index]`.
    let priorBitFieldNameCount (bfs: BAREWire.Hardware.BitFieldDescriptor array) (index: int) : int =
        let name = bfs.[index].Name
        let mutable count = 0
        let mutable j = 0
        while j < index do
            if bfs.[j].Name = name then count <- count + 1
            j <- j + 1
        count

    let bitFieldsValid (field: BAREWire.Hardware.FieldDescriptor) : bool =
        let width = viewElementWidth field.Repr
        let bits = int64 width * 8L
        let bfs = field.BitFields
        let mutable ok = width > 0
        let mutable i = 0
        while ok && i < bfs.Length do
            let bf = bfs.[i]
            let posI = int64 bf.Position
            let widI = int64 bf.Width
            let top = posI + widI
            let dup = priorBitFieldNameCount bfs i > 0
            let mutable overlap = false
            let mutable j = 0
            while j < i && not overlap do
                let posJ = int64 bfs.[j].Position
                let widJ = int64 bfs.[j].Width
                overlap <- not (posI + widI <= posJ || posJ + widJ <= posI)
                j <- j + 1
            ok <- trimmedLength bf.Name > 0 && not dup && not overlap
                  && BAREWire.Hardware.AccessKind.isValid bf.Access
                  && bf.Position >= 0 && bf.Width >= 1 && top <= bits
            i <- i + 1
        ok

    /// Count of struct fields with blank/duplicate names or invalid bitfields.
    let invalidFieldMetaCount (structDesc: BAREWire.Hardware.StructDescriptor) : int =
        let fields = structDesc.Layout.Fields
        let mutable bad = 0
        let mutable i = 0
        while i < fields.Length do
            let f = fields.[i]
            let dup = priorFieldNameCount fields i > 0
            if trimmedLength f.Name = 0 || dup || not (bitFieldsValid f) then
                bad <- bad + 1
            i <- i + 1
        bad

    /// Pairwise field non-overlap when MayAlias = false.
    let structFieldsNonOverlapping (structDesc: BAREWire.Hardware.StructDescriptor) : bool =
        if structDesc.MayAlias then true
        else
            let fields = structDesc.Layout.Fields
            let mutable ok = true
            let mutable i = 0
            while ok && i < fields.Length do
                let fi = fields.[i]
                let wi = int64 (viewElementWidth fi.Repr)
                let li = int64 fi.Count * wi
                let oi = int64 fi.Offset
                let mutable j = i + 1
                while ok && j < fields.Length do
                    let fj = fields.[j]
                    let wj = int64 (viewElementWidth fj.Repr)
                    let lj = int64 fj.Count * wj
                    let oj = int64 fj.Offset
                    ok <- wi > 0L && wj > 0L && li > 0L && lj > 0L && (oi + li <= oj || oj + lj <= oi)
                    j <- j + 1
                i <- i + 1
            ok

    let structFieldsGeometryValid (p: SharedBufferPlacement) : bool =
        let layout = p.StructLayout.Layout
        layout.Fields.Length > 0
        && not (structHasFlexibleArray p.StructLayout)
        && invalidFieldMetaCount p.StructLayout = 0
        && structFieldsNonOverlapping p.StructLayout
        && (layout.Fields |> Array.forall (fun field ->
            let width = viewElementWidth field.Repr
            field.Count > 0 && width > 0 && field.Offset >= 0
            && int64 field.Offset + int64 width * int64 field.Count <= int64 layout.Size
            && field.Offset % width = 0 && p.LayoutAlignment >= width))

    let structFieldAccessMatches (p: SharedBufferPlacement) : bool =
        p.StructLayout.Layout.Fields |> Array.forall (fun field ->
            match p.SpaceDescriptor.Access with
            | "ReadOnly" -> field.Access = BAREWire.Hardware.AccessKind.ReadOnly
            | "WriteOnly" -> field.Access = BAREWire.Hardware.AccessKind.WriteOnly
            | "ReadWrite" | "ExclusiveMutable" ->
                field.Access = BAREWire.Hardware.AccessKind.ReadOnly || field.Access = BAREWire.Hardware.AccessKind.ReadWrite
            | _ -> false)

    let structFieldsValid (p: SharedBufferPlacement) : bool =
        structFieldsGeometryValid p && structFieldAccessMatches p

    let viewLayoutValid (p: SharedBufferPlacement) : bool =
        trimmedLength p.ViewLayout.Schema > 0
        && viewElementFits p
        && p.ViewLayout.Alignment > 0 && p.ViewLayout.Alignment = p.LayoutAlignment

    let viewAccessMatches (p: SharedBufferPlacement) : bool =
        match p.SpaceDescriptor.Access with
        | "ReadOnly" -> p.ViewLayout.Access = BAREWire.Hardware.AccessKind.ReadOnly
        | "WriteOnly" -> p.ViewLayout.Access = BAREWire.Hardware.AccessKind.WriteOnly
        | "ReadWrite" | "ExclusiveMutable" -> p.ViewLayout.Access = BAREWire.Hardware.AccessKind.ReadWrite
        | _ -> false

    let spaceKindMatches (kind: MemoryKind) (region: BAREWire.Hardware.MemoryRegionKind) : bool =
        match kind with
        | MemoryKind.Sram -> region = BAREWire.Hardware.MemoryRegionKind.SRAM
        | MemoryKind.Dma -> region = BAREWire.Hardware.MemoryRegionKind.DMA
        | MemoryKind.Flash -> region = BAREWire.Hardware.MemoryRegionKind.Flash
        | MemoryKind.Peripheral -> region = BAREWire.Hardware.MemoryRegionKind.Peripheral
        | MemoryKind.ViewBacked ->
            region = BAREWire.Hardware.MemoryRegionKind.SRAM || region = BAREWire.Hardware.MemoryRegionKind.DMA
        | _ -> false

    let spaceAlignmentValid (alignment: int) (regionAlignment: int) : bool =
        isPowerOfTwo alignment && regionAlignment > 0
        && regionAlignment <= alignment && alignment % regionAlignment = 0

    let spaceAccessMatches (access: Access) (declaredAccess: string) : bool =
        match declaredAccess with
        | "ReadOnly" -> access = Access.ReadOnly || access = Access.ReadWrite
        | "WriteOnly" -> access = Access.WriteOnly || access = Access.ReadWrite
        | "ReadWrite" | "ExclusiveMutable" -> access = Access.ReadWrite
        | _ -> false

    let priorNameCount (placements: SharedBufferPlacement array) (index: int) : int =
        let target = trimmedText placements.[index].Name
        let mutable count = 0
        let mutable i = 0
        while i < index do
            if trimmedText placements.[i].Name = target then
                count <- count + 1
            i <- i + 1
        count

    let internal nameSeenBefore (placements: SharedBufferPlacement array) (index: int) : bool =
        priorNameCount placements index > 0

/// The platform's C calling convention and its scalar aggregate register limit.
/// Vector and other special ABI classes require separate classification.
type CAbiDescriptor = {
    Name: string
    PointerBits: int
    ScalarAggregateRegisterBytes: int
}

/// A selected foreign code residence assumption, separate from runtime
/// availability, ABI widths and compiler-owned program-image lifetime.
/// Consumers accept only explicitly supported policies; no omitted field
/// supplies a loading or unloading default.
type ProcessLinkContract = {
    Identity: string
    Version: string
    Source: string
    Library: string
    Runtime: string
    Os: string
    Arch: string
    LoadAt: string
    LiveUntil: string
    Unloading: string
    ExportedLibrary: string
}

/// Declared provenance and symbol visibility for a statically linked archive
/// (`.a` / LLVM bitcode archive) bound through `ld.lld` LTO (Farscape #204).
/// `ArchivePath` is relative to the platform profile root (or an explicit rooted path);
/// `ArchiveSource` identifies the archive build or origin; `Sha256` is the 64-character
/// ASCII hexadecimal digest of the archive bytes; `Format` names the member payload kind
/// (`ArchiveFormat`); and `Visibility` states the required resulting symbol visibility
/// (`SymbolVisibility`) for symbols resolved from the archive.
type StaticArchiveContract = {
    ArchivePath: string
    ArchiveSource: string
    Sha256: string
    Format: ArchiveFormat
    Visibility: SymbolVisibility
}

/// Process-link specification uniting dynamic shared-object (`.so`) and static archive
/// (`.a` / LTO bitcode) contracts under `ProcessLinkContract` while keeping `[<FidelityExtern>]`
/// declarations invariant (`Static Binding Elision Test`, Farscape #204).
/// `DynamicSharedObject` names the expected runtime SONAME/path for dynamic links
/// (or `""` when implied solely by `Contract.Library`); `StaticArchive` carries `Some`
/// when `LinkKind = LinkKind.StaticArchive` and `None` when `LinkKind = LinkKind.DynamicSharedObject`.
type ProcessLinkSpec = {
    Contract: ProcessLinkContract
    LinkKind: LinkKind
    DynamicSharedObject: string
    StaticArchive: StaticArchiveContract option
}

/// Constructors for static archive link contracts.
module StaticArchiveContract =

    /// Declare a static archive contract with explicit path, origin, SHA-256 digest,
    /// member payload format, and required resulting symbol visibility.
    let create (archivePath: string) (archiveSource: string) (sha256: string) (format: ArchiveFormat) (visibility: SymbolVisibility) : StaticArchiveContract =
        { ArchivePath = archivePath
          ArchiveSource = archiveSource
          Sha256 = sha256
          Format = format
          Visibility = visibility }

/// Constructors for process-link specifications.
module ProcessLinkSpec =

    /// Wrap an existing dynamic `ProcessLinkContract` with no explicit SONAME override.
    let dynamic (contract: ProcessLinkContract) : ProcessLinkSpec =
        { Contract = contract
          LinkKind = LinkKind.DynamicSharedObject
          DynamicSharedObject = ""
          StaticArchive = None }

    /// Wrap a dynamic `ProcessLinkContract` naming the expected runtime shared-object path or SONAME.
    let dynamicWithSharedObject (contract: ProcessLinkContract) (sharedObject: string) : ProcessLinkSpec =
        { Contract = contract
          LinkKind = LinkKind.DynamicSharedObject
          DynamicSharedObject = sharedObject
          StaticArchive = None }

    /// Pair a `ProcessLinkContract` with an explicit static archive contract (`.a` / LTO bitcode).
    let staticArchive (contract: ProcessLinkContract) (archive: StaticArchiveContract) : ProcessLinkSpec =
        { Contract = contract
          LinkKind = LinkKind.StaticArchive
          DynamicSharedObject = ""
          StaticArchive = Some archive }

/// The platform description: the declared authority on memory layout,
/// boundary surfaces, buffer capacities, and transports for one processor
/// (docs/11). A Fidelity.Platform target tree is one `PlatformDescription`
/// value; the compiler extracts the same value structurally from the PSG and
/// runs the observers in `Check`, `Manifest`, and `Obligations` on it. Every
/// table is an array in declaration order, every tag a string alias from
/// `Tags.fs`, and every function here is total.

/// One memory space: a region of the address space, or of a device, with a
/// declared capacity, alignment, growth direction and permissions. `Base` is
/// the declared origin when the space has one (a section, a BRAM, a register
/// bank) and `None` when the loader or allocator assigns it. `Granularity`
/// is the allocation or access unit in bytes (a page for a heap, a word for a
/// register bank, one for a byte-addressed arena).
type MemorySpace = {
    Name: string
    Kind: MemoryKind
    Capacity: int64
    Alignment: int
    Granularity: int
    Growth: Growth
    Access: Access
    Base: int64 option
    Notes: string
    /// For a space of kind Map, the map kind; "" otherwise.
    MapKind: MapKind
    /// Host version from which the space exists; "" when unbounded.
    Since: string
    /// Host version from which the space is gone; "" when unbounded.
    Until: string
}

/// A contract on a boundary: a statement, its standing (`Logic.Assumed` for
/// a trusted-base fact on the far side, or the SMT logic it is decided in),
/// and the weakness ids it guards against (`"CWE-120"`). A contract that
/// bounds the endpoint's return declares the bound as data, not prose
/// (docs/11, "The TCB, mechanized"; clef Dimensional_Range_Design.md, ruling
/// 2 of CS-12): `Floor` is the least value the return takes (on Linux the
/// errno floor, -4095, since a negative return is an errno), and `AtMost`
/// names the parameter the return is at most (`"count"` for read and write),
/// so the return's declared range is [Floor, hi(AtMost)]. `AtMost = ""`
/// declares no return bound, and `Floor` is then not read: the compiler
/// leaves such a return unobservable (CCS8011) and never supplies a number.
type Contract = {
    Name: string
    Statement: string
    Logic: Logic
    Refs: string array
    Floor: int64
    AtMost: string
}

/// One endpoint of a surface: a syscall with its number, a pin with its
/// package location, a symbol, a path, a port. `Address` is the location as
/// text: a syscall number, a pin name, a path, a symbol.
type Endpoint = {
    Name: string
    Location: EndpointKind
    Address: string
    Contracts: Contract array
    /// The endpoint's signature where one is declared: a hook's context type
    /// and return convention (`xdp_md -> xdp_action`), a helper's parameter
    /// list, a syscall's argument shape; "" when none.
    Signature: string
    /// Host version from which the endpoint exists (a helper that arrived in
    /// kernel 5.17 is `Since = "5.17"`); "" when unbounded.
    Since: string
    /// Host version from which the endpoint is gone; "" when unbounded.
    Until: string
}

/// A surface-wide default, such as an IO standard for every pin.
type Default = {
    Key: string
    Value: string
}

/// A boundary surface: the syscall table, an FFI, a host API, a pin map, or
/// an IPC channel, with its endpoints, defaults, and surface-wide contracts.
type BoundarySurface = {
    Name: string
    Kind: SurfaceKind
    Endpoints: Endpoint array
    Defaults: Default array
    Contracts: Contract array
}

/// A declared buffer: the BARE type it carries (by name), its capacity in
/// bytes, how its content is framed, which space it lives in, and how long
/// it lives. `Delimiter` is the delimiter byte value for `Framing.Delimited`
/// and `-1` otherwise; `TrimDelimiter` says whether the delimiter is dropped
/// from the value handed on. This is the declared home of facts such as
/// `Console.readln`'s 1024 bytes and newline trim (Readiness Audit §2).
type BufferSchema = {
    Name: string
    Schema: string
    Capacity: int64
    Framing: Framing
    Delimiter: int
    TrimDelimiter: bool
    Space: string
    Lifetime: Lifetime
    Access: Access
    /// For `Ring` framing, the slot size in bytes; 0 otherwise.
    Slot: int
}

/// A transport: the medium, the endpoints it binds (by endpoint name), its
/// declared rate and largest unit (0 when undeclared), whether units arrive
/// in order, and the BARE type of a unit ("" when none).
type Transport = {
    Name: string
    Kind: TransportKind
    Endpoints: string array
    RateHz: int64
    MaxUnit: int64
    Ordered: bool
    Schema: string
    /// Host version from which the transport exists; "" when unbounded.
    Since: string
    /// Host version from which the transport is gone; "" when unbounded.
    Until: string
}

/// A clock and its frequency.
type Clock = {
    Name: string
    FrequencyHz: int64
}

/// A reset line: external or internal, and its active level.
type Reset = {
    Name: string
    External: bool
    ActiveHigh: bool
}

/// How the platform starts, stops, and keeps state: clocks and resets (a
/// device), the entry and teardown symbols (a process), and persistence.
type LifecycleFacts = {
    Clocks: Clock array
    Resets: Reset array
    Entry: string
    Teardown: string
    Persistence: Persistence
}

/// One named width dimension the core declares (clef-lang-spec
/// ntu-dimensional-architecture.md §7.1): the language names a dimension
/// (`Pointer`, `Register`, a lane, an accumulator) and the description says
/// how many bits it is on this target. A dimension the description does not
/// declare has no width: the compiler reports the site that needed it and
/// never supplies a number of its own (plan D1, D8).
type WidthDeclaration = {
    Name: string
    Bits: int
}

/// One numeric representation the target offers (platform-bindings.md,
/// descriptor requirements; numeric-selection.md §7, §9 item 6). The
/// language carries only the name (`int32`, `float64`, `posit32`); what
/// that name is on this target is declared here. `Capability`, `Family`
/// and `Boundary` are tags from `Tags.fs`. `MinMagnitude` and
/// `MaxMagnitude` bound the representable range, the least and the greatest
/// finite value, as exact decimal text: the group is Z, so a bound is a
/// declared number, never a float the host rounds.
type Representation = {
    Name: string
    Capability: Capability
    Family: RepresentationFamily
    Bits: int
    MinMagnitude: string
    MaxMagnitude: string
    Boundary: Boundary
}

/// The ISA and ABI identity of a processor core: the `TargetCore` block of
/// `Fidelity.Platform/docs/CANONICAL_PLATFORM_SPEC.md`, with the width
/// dimensions and the numeric representations it declares (plan D8).
/// `Triple` and `CpuModel` are "" when the toolchain derives them.
type TargetCore = {
    Os: string
    Arch: string
    WordSizeBits: int
    Endianness: string
    Runtime: string
    Triple: string
    CpuModel: string
    /// The width dimensions by name: a CPU declares `Pointer` and `Register`.
    Widths: WidthDeclaration array
    /// The numeric representations the target offers, each with its
    /// capability, range and boundary semantics.
    Representations: Representation array
}

/// One named numeric limit the host imposes.
type Limit = {
    Name: string
    Value: int64
}

/// Named authority for program-lifetime storage. These names reference Spaces;
/// they do not infer placement from a space's kind or duplicate its budget.
/// Immutable denotes an immutable image, not permission to initialize a source
/// immutable binding by writing into read-only memory at runtime.
type ProgramLifetimeSpaces = {
    Immutable: string
    Mutable: string option
}

/// The whole description of one processor: identity, the core's ISA facts
/// when it has a core, and the declared spaces, surfaces, buffers, transports,
/// lifecycle, and limits the three observers read.
type PlatformDescription = {
    Id: string
    DisplayName: string
    Substrate: string
    Core: TargetCore option
    Spaces: MemorySpace array
    /// None supplies no program-lifetime storage authority. Consumers requiring
    /// that storage must reject absence; they must not select a space by name.
    ProgramLifetime: ProgramLifetimeSpaces option
    Surfaces: BoundarySurface array
    Buffers: BufferSchema array
    Transports: Transport array
    Lifecycle: LifecycleFacts
    Notes: string array
    /// Declared numeric limits of the host: a verifier's stack bytes and
    /// instruction budget, an ISA level, a message cap. Named so an
    /// obligation can cite them.
    Limits: Limit array
}

/// Lookups over a description. Each is a linear scan in declaration order and
/// returns the first match; `Check.run` reports duplicate names.
module PlatformDescription =

    let private indexOfSpace (spaces: MemorySpace array) (name: string) : int =
        let n = Array.length spaces
        let mutable i = 0
        let mutable found = -1
        while found < 0 && i < n do
            let s = Array.get spaces i
            if s.Name = name then found <- i
            i <- i + 1
        found

    let private indexOfBuffer (buffers: BufferSchema array) (name: string) : int =
        let n = Array.length buffers
        let mutable i = 0
        let mutable found = -1
        while found < 0 && i < n do
            let b = Array.get buffers i
            if b.Name = name then found <- i
            i <- i + 1
        found

    let private indexOfSurface (surfaces: BoundarySurface array) (name: string) : int =
        let n = Array.length surfaces
        let mutable i = 0
        let mutable found = -1
        while found < 0 && i < n do
            let s = Array.get surfaces i
            if s.Name = name then found <- i
            i <- i + 1
        found

    let private indexOfTransport (transports: Transport array) (name: string) : int =
        let n = Array.length transports
        let mutable i = 0
        let mutable found = -1
        while found < 0 && i < n do
            let t = Array.get transports i
            if t.Name = name then found <- i
            i <- i + 1
        found

    let private indexOfEndpoint (endpoints: Endpoint array) (name: string) : int =
        let n = Array.length endpoints
        let mutable i = 0
        let mutable found = -1
        while found < 0 && i < n do
            let e = Array.get endpoints i
            if e.Name = name then found <- i
            i <- i + 1
        found

    /// The memory space with this name.
    let tryFindSpace (desc: PlatformDescription) (name: string) : MemorySpace option =
        let i = indexOfSpace desc.Spaces name
        if i >= 0 then Some (Array.get desc.Spaces i) else None

    /// The buffer schema with this name.
    let tryFindBuffer (desc: PlatformDescription) (name: string) : BufferSchema option =
        let i = indexOfBuffer desc.Buffers name
        if i >= 0 then Some (Array.get desc.Buffers i) else None

    /// The boundary surface with this name.
    let tryFindSurface (desc: PlatformDescription) (name: string) : BoundarySurface option =
        let i = indexOfSurface desc.Surfaces name
        if i >= 0 then Some (Array.get desc.Surfaces i) else None

    /// The transport with this name.
    let tryFindTransport (desc: PlatformDescription) (name: string) : Transport option =
        let i = indexOfTransport desc.Transports name
        if i >= 0 then Some (Array.get desc.Transports i) else None

    /// The endpoint with this name on the named surface.
    let tryFindEndpoint (desc: PlatformDescription) (surfaceName: string) (endpointName: string) : Endpoint option =
        let si = indexOfSurface desc.Surfaces surfaceName
        let endpoints = if si >= 0 then (Array.get desc.Surfaces si).Endpoints else Array.zeroCreate 0
        let ei = indexOfEndpoint endpoints endpointName
        if ei >= 0 then Some (Array.get endpoints ei) else None

    /// True when some surface declares an endpoint with this name; this is
    /// how a transport's endpoint references resolve.
    let hasEndpoint (desc: PlatformDescription) (endpointName: string) : bool =
        let n = Array.length desc.Surfaces
        let mutable i = 0
        let mutable found = false
        while not found && i < n do
            let s = Array.get desc.Surfaces i
            found <- indexOfEndpoint s.Endpoints endpointName >= 0
            i <- i + 1
        found

    /// The memory space a buffer lives in.
    let spaceOfBuffer (desc: PlatformDescription) (buffer: BufferSchema) : MemorySpace option =
        tryFindSpace desc buffer.Space

    /// The declared capacity of the named buffer, or 0 when there is none.
    let capacityOf (desc: PlatformDescription) (bufferName: string) : int64 =
        let i = indexOfBuffer desc.Buffers bufferName
        if i >= 0 then (Array.get desc.Buffers i).Capacity else 0L

    /// The declared capacity of the named space, or 0 when there is none.
    let spaceCapacityOf (desc: PlatformDescription) (spaceName: string) : int64 =
        let i = indexOfSpace desc.Spaces spaceName
        if i >= 0 then (Array.get desc.Spaces i).Capacity else 0L

    let private indexOfLimit (limits: Limit array) (name: string) : int =
        let n = Array.length limits
        let mutable i = 0
        let mutable found = -1
        while found < 0 && i < n do
            let l = Array.get limits i
            if l.Name = name then found <- i
            i <- i + 1
        found

    /// The limit with this name.
    let tryFindLimit (desc: PlatformDescription) (name: string) : Limit option =
        let i = indexOfLimit desc.Limits name
        if i >= 0 then Some (Array.get desc.Limits i) else None

    /// The value of the named limit, or 0 when there is none.
    let limitOf (desc: PlatformDescription) (name: string) : int64 =
        let i = indexOfLimit desc.Limits name
        if i >= 0 then (Array.get desc.Limits i).Value else 0L

    /// The first declared BufferSchema in `p.SpaceDescriptor.Space` whose `Name` or `Schema`
    /// matches `p.ViewLayout.Schema`.
    let tryFindSharedBufferSchema (desc: PlatformDescription) (p: SharedBufferPlacement) : BufferSchema option =
        let n = Array.length desc.Buffers
        let schema = p.ViewLayout.Schema
        let spaceName = p.SpaceDescriptor.Space
        let mutable i = 0
        let mutable found = -1
        while found < 0 && i < n do
            let b = Array.get desc.Buffers i
            if SharedBufferPlacement.trimmedLength b.Name > 0 && b.Space = spaceName && (b.Name = schema || b.Schema = schema) then
                found <- i
            i <- i + 1
        if found >= 0 then Some (Array.get desc.Buffers found) else None

    /// Non-zero when either `desc.Buffers` is empty and `p.ViewLayout.Schema` is non-blank,
    /// or `tryFindSharedBufferSchema` resolves a named buffer in `p.SpaceDescriptor.Space`.
    let matchedBufferSchemaLength (desc: PlatformDescription) (p: SharedBufferPlacement) : int =
        if Array.length desc.Buffers = 0 then
            SharedBufferPlacement.trimmedLength p.ViewLayout.Schema
        else
            match tryFindSharedBufferSchema desc p with
            | Some b -> SharedBufferPlacement.trimmedLength b.Name
            | None -> 0

/// Host versions as dotted numerals ("5.17", "6.1.0"), and the version-ranged
/// availability every endpoint, space, and transport carries: the capability
/// matrix a project's pinned host floor is filtered against (docs/11, "The
/// kernel as a described platform"). A component is available at a host when
/// `Since` is empty or at most the host, and `Until` is empty or greater
/// than the host.
module Availability =

    /// Compare two dotted version strings numerically, component by
    /// component; a missing component is 0. Negative when `a < b`, zero when
    /// equal, positive when `a > b`. Non-digit characters end a component.
    let compareVersions (a: string) (b: string) : int =
        let la = String.length a
        let lb = String.length b
        let mutable ia = 0
        let mutable ib = 0
        let mutable result = 0
        let mutable fin = false
        while not fin do
            let mutable va = 0
            while ia < la && Text.charAt a ia >= '0' && Text.charAt a ia <= '9' do
                va <- va * 10 + (int (Text.charAt a ia) - 48)
                ia <- ia + 1
            let mutable vb = 0
            while ib < lb && Text.charAt b ib >= '0' && Text.charAt b ib <= '9' do
                vb <- vb * 10 + (int (Text.charAt b ib) - 48)
                ib <- ib + 1
            result <- (if va < vb then -1 elif va > vb then 1 else 0)
            let moreA = ia < la
            let moreB = ib < lb
            // skip one separator on each side
            ia <- (if moreA then ia + 1 else ia)
            ib <- (if moreB then ib + 1 else ib)
            fin <- result <> 0 || (not moreA && not moreB)
        result

    /// True when a component available from `since` until `until` exists on
    /// the host version. Empty bounds are unbounded.
    let admits (since: string) (until: string) (host: string) : bool =
        let afterSince = String.length since = 0 || compareVersions since host <= 0
        let beforeUntil = String.length until = 0 || compareVersions host until < 0
        afterSince && beforeUntil

    /// True when the range is well-formed: `until` empty, or `since` empty, or
    /// `since` before `until`.
    let wellFormed (since: string) (until: string) : bool =
        String.length since = 0 || String.length until = 0 || compareVersions since until < 0

/// Constructors for memory spaces. `create` gives a byte-granular,
/// read-write, fixed-extent space with no declared base; the `with*`
/// functions refine one field at a time.
module MemorySpace =

    /// A space with the given name, kind, capacity and alignment; granularity
    /// 1, fixed extent, read-write, no declared base.
    let create (name: string) (kind: MemoryKind) (capacity: int64) (alignment: int) : MemorySpace =
        { Name = name; Kind = kind; Capacity = capacity; Alignment = alignment; Granularity = 1
          Growth = Growth.Fixed; Access = Access.ReadWrite; Base = None; Notes = ""; MapKind = ""; Since = ""; Until = "" }

    /// A kernel map: a space of kind Map with its map kind, byte-granular,
    /// read-write, no base (the kernel places it).
    let map (name: string) (mapKind: MapKind) (capacity: int64) (alignment: int) : MemorySpace =
        { Name = name; Kind = MemoryKind.Map; Capacity = capacity; Alignment = alignment; Granularity = 1
          Growth = Growth.Fixed; Access = Access.ReadWrite; Base = None; Notes = ""; MapKind = mapKind; Since = ""; Until = "" }

    /// The same space available from `since` until `until` ("" unbounded).
    let withAvailability (space: MemorySpace) (since: string) (until: string) : MemorySpace =
        { space with Since = since; Until = until }

    /// The same space with a declared base address.
    let withBase (space: MemorySpace) (origin: int64) : MemorySpace =
        { space with Base = Some origin }

    /// The same space with a growth direction.
    let withGrowth (space: MemorySpace) (growth: Growth) : MemorySpace =
        { space with Growth = growth }

    /// The same space with permissions.
    let withAccess (space: MemorySpace) (access: Access) : MemorySpace =
        { space with Access = access }

    /// The same space with an allocation or access granularity.
    let withGranularity (space: MemorySpace) (granularity: int) : MemorySpace =
        { space with Granularity = granularity }

    /// The same space with a note.
    let withNotes (space: MemorySpace) (notes: string) : MemorySpace =
        { space with Notes = notes }

    /// The last byte offset plus one of a space with a declared base, or 0.
    let end' (space: MemorySpace) : int64 =
        let b =
            match space.Base with
            | Some v -> v + space.Capacity
            | None -> 0L
        b

/// Constructors for buffer schemas. Each names the framing it declares;
/// buffers default to scope lifetime and read-write access.
module BufferSchema =

    /// A buffer of a fixed extent: the whole capacity is the value. (Spelled
    /// with a prime because `fixed` is a keyword.)
    let fixed' (name: string) (schema: string) (capacity: int64) (space: string) : BufferSchema =
        { Name = name; Schema = schema; Capacity = capacity; Framing = Framing.Fixed; Delimiter = -1
          TrimDelimiter = false; Space = space; Lifetime = Lifetime.Scope; Access = Access.ReadWrite; Slot = 0 }

    /// A buffer whose content carries a length prefix.
    let lengthPrefixed (name: string) (schema: string) (capacity: int64) (space: string) : BufferSchema =
        { Name = name; Schema = schema; Capacity = capacity; Framing = Framing.LengthPrefixed; Delimiter = -1
          TrimDelimiter = false; Space = space; Lifetime = Lifetime.Scope; Access = Access.ReadWrite; Slot = 0 }

    /// A buffer whose content ends at a delimiter byte, dropped when `trim`.
    let delimited (name: string) (schema: string) (capacity: int64) (delimiter: int) (trim: bool) (space: string) : BufferSchema =
        { Name = name; Schema = schema; Capacity = capacity; Framing = Framing.Delimited; Delimiter = delimiter
          TrimDelimiter = trim; Space = space; Lifetime = Lifetime.Scope; Access = Access.ReadWrite; Slot = 0 }

    /// A ring of fixed slots over a space of kind Ring: `capacity` is the ring
    /// size in bytes, `slot` the slot size, consumed through producer and
    /// consumer indices.
    let ring (name: string) (schema: string) (capacity: int64) (slot: int) (space: string) : BufferSchema =
        { Name = name; Schema = schema; Capacity = capacity; Framing = Framing.Ring; Delimiter = -1
          TrimDelimiter = false; Space = space; Lifetime = Lifetime.Session; Access = Access.ReadWrite; Slot = slot }

    /// A buffer bounded only by its capacity, with no in-band marker.
    let capped (name: string) (schema: string) (capacity: int64) (space: string) : BufferSchema =
        { Name = name; Schema = schema; Capacity = capacity; Framing = Framing.Capped; Delimiter = -1
          TrimDelimiter = false; Space = space; Lifetime = Lifetime.Scope; Access = Access.ReadWrite; Slot = 0 }

    /// The same buffer with a lifetime.
    let withLifetime (buffer: BufferSchema) (lifetime: Lifetime) : BufferSchema =
        { buffer with Lifetime = lifetime }

    /// The same buffer with permissions.
    let withAccess (buffer: BufferSchema) (access: Access) : BufferSchema =
        { buffer with Access = access }

    /// True when the buffer's framing carries a delimiter and one is declared.
    let hasDelimiter (buffer: BufferSchema) : bool =
        Framing.hasDelimiter buffer.Framing && buffer.Delimiter >= 0 && buffer.Delimiter <= 255

/// Constructors for contracts.
module Contract =

    /// A trusted-base assumption: the far side of a boundary, stated and
    /// cited but not proven (docs/11, "The TCB, mechanized").
    let assumed (name: string) (statement: string) (refs: string array) : Contract =
        { Name = name; Statement = statement; Logic = Logic.Assumed; Refs = refs; Floor = 0L; AtMost = "" }

    /// A contract decided in the given logic.
    let proven (name: string) (statement: string) (logic: Logic) (refs: string array) : Contract =
        { Name = name; Statement = statement; Logic = logic; Refs = refs; Floor = 0L; AtMost = "" }

    /// The same contract with the endpoint's return bounded: at least `floor`,
    /// at most the parameter `atMost` names.
    let withReturnBound (contract: Contract) (floor: int64) (atMost: string) : Contract =
        { contract with Floor = floor; AtMost = atMost }

    /// True when the contract declares a return bound.
    let boundsReturn (contract: Contract) : bool =
        String.length contract.AtMost > 0

/// Constructors for endpoints and surfaces.
module Endpoint =

    /// An endpoint at a location with its contracts.
    let create (name: string) (location: EndpointKind) (address: string) (contracts: Contract array) : Endpoint =
        { Name = name; Location = location; Address = address; Contracts = contracts; Signature = ""; Since = ""; Until = "" }

    /// A syscall endpoint: the number as text, with its contracts.
    let syscall (name: string) (number: int) (contracts: Contract array) : Endpoint =
        { Name = name; Location = EndpointKind.SyscallNumber; Address = BAREWire.Encoding.Fmt.ofInt number; Contracts = contracts; Signature = ""; Since = ""; Until = "" }

    /// A numbered host helper, available from a host version: the BPF helper
    /// table's shape. The address is the helper number as text.
    let helper (name: string) (number: int) (since: string) (contracts: Contract array) : Endpoint =
        { Name = name; Location = EndpointKind.HelperNumber; Address = BAREWire.Encoding.Fmt.ofInt number; Contracts = contracts; Signature = ""; Since = since; Until = "" }

    /// The same endpoint available from `since` until `until` ("" unbounded).
    let withAvailability (endpoint: Endpoint) (since: string) (until: string) : Endpoint =
        { endpoint with Since = since; Until = until }

    /// The same endpoint with a declared signature.
    let withSignature (endpoint: Endpoint) (signature: string) : Endpoint =
        { endpoint with Signature = signature }

    /// An attach point on a host API surface: a hook is a pin (docs/11). The
    /// signature is the context type it hands the program and the return
    /// convention it expects, as text.
    let hook (name: string) (signature: string) (since: string) : Endpoint =
        { Name = name; Location = EndpointKind.Symbol; Address = name; Contracts = Array.zeroCreate 0; Signature = signature; Since = since; Until = "" }

/// Constructors for boundary surfaces.
module BoundarySurface =

    /// A surface with endpoints and no defaults or surface-wide contracts.
    let create (name: string) (kind: SurfaceKind) (endpoints: Endpoint array) : BoundarySurface =
        { Name = name; Kind = kind; Endpoints = endpoints; Defaults = Array.zeroCreate 0; Contracts = Array.zeroCreate 0 }

    /// The same surface with defaults.
    let withDefaults (surface: BoundarySurface) (defaults: Default array) : BoundarySurface =
        { surface with Defaults = defaults }

    /// The same surface with surface-wide contracts.
    let withContracts (surface: BoundarySurface) (contracts: Contract array) : BoundarySurface =
        { surface with Contracts = contracts }

/// Constructors for transports.
module Transport =

    /// A transport over a medium binding the named endpoints; rate and unit
    /// undeclared, ordered, no schema.
    let create (name: string) (kind: TransportKind) (endpoints: string array) : Transport =
        { Name = name; Kind = kind; Endpoints = endpoints; RateHz = 0L; MaxUnit = 0L; Ordered = true; Schema = ""; Since = ""; Until = "" }

    /// The same transport available from `since` until `until` ("" unbounded).
    let withAvailability (transport: Transport) (since: string) (until: string) : Transport =
        { transport with Since = since; Until = until }

    /// The same transport with a declared rate.
    let withRate (transport: Transport) (rateHz: int64) : Transport =
        { transport with RateHz = rateHz }

    /// The same transport with a largest unit.
    let withMaxUnit (transport: Transport) (maxUnit: int64) : Transport =
        { transport with MaxUnit = maxUnit }

    /// The same transport carrying the named BARE type.
    let withSchema (transport: Transport) (schema: string) : Transport =
        { transport with Schema = schema }

    /// The same transport with its ordering guarantee.
    let withOrdered (transport: Transport) (ordered: bool) : Transport =
        { transport with Ordered = ordered }

/// Constructors for lifecycle facts.
module Lifecycle =

    /// A process lifecycle: entry and teardown symbols, no clocks or resets.
    let process' (entry: string) (teardown: string) (persistence: Persistence) : LifecycleFacts =
        { Clocks = Array.zeroCreate 0; Resets = Array.zeroCreate 0; Entry = entry; Teardown = teardown; Persistence = persistence }

    /// A device lifecycle: clocks and resets, no entry or teardown.
    let device (clocks: Clock array) (resets: Reset array) (persistence: Persistence) : LifecycleFacts =
        { Clocks = clocks; Resets = resets; Entry = ""; Teardown = ""; Persistence = persistence }

    /// A clock.
    let clock (name: string) (frequencyHz: int64) : Clock =
        { Name = name; FrequencyHz = frequencyHz }

    /// A reset line.
    let reset (name: string) (external: bool) (activeHigh: bool) : Reset =
        { Name = name; External = external; ActiveHigh = activeHigh }

/// Constructors for the core identity block. The block follows
/// Fidelity.Platform's `CANONICAL_PLATFORM_SPEC.md` `TargetCore`; its two
/// optional fields (`TripleOverride`, `CpuModel`) are flat strings here, ""
/// when derived by the toolchain, so the record stays a plain table.
module TargetCore =

    /// A core with the toolchain deriving the triple and CPU model, and no
    /// width or representation declared yet; `withWidths` and
    /// `withRepresentations` add them. An undeclared width is not a
    /// default: the compiler refuses a site that needs it.
    let create (os: string) (arch: string) (wordSizeBits: int) (endianness: string) (runtime: string) : TargetCore =
        { Os = os; Arch = arch; WordSizeBits = wordSizeBits; Endianness = endianness; Runtime = runtime; Triple = ""; CpuModel = ""
          Widths = Array.zeroCreate 0; Representations = Array.zeroCreate 0 }

    /// The same core declaring these width dimensions.
    let withWidths (core: TargetCore) (widths: WidthDeclaration array) : TargetCore =
        { core with Widths = widths }

    /// The same core offering these numeric representations.
    let withRepresentations (core: TargetCore) (representations: Representation array) : TargetCore =
        { core with Representations = representations }

    /// The same core with an explicit triple.
    let withTriple (core: TargetCore) (triple: string) : TargetCore =
        { core with Triple = triple }

    /// The same core with an explicit CPU model.
    let withCpuModel (core: TargetCore) (cpuModel: string) : TargetCore =
        { core with CpuModel = cpuModel }

/// Constructors for width dimensions.
module WidthDeclaration =

    /// The dimension with this name at this many bits.
    let create (name: string) (bits: int) : WidthDeclaration =
        { Name = name; Bits = bits }

    /// The address width; a CPU declares it.
    [<Literal>]
    let Pointer = "Pointer"

    /// The natural computational word; a CPU declares it.
    [<Literal>]
    let Register = "Register"

/// Constructors for numeric representations. Each names the family it
/// declares; the range bounds are exact decimal text.
module Representation =

    /// A representation of any family, every field stated.
    let create (name: string) (capability: Capability) (family: RepresentationFamily) (bits: int) (minMagnitude: string) (maxMagnitude: string) (boundary: Boundary) : Representation =
        { Name = name; Capability = capability; Family = family; Bits = bits; MinMagnitude = minMagnitude; MaxMagnitude = maxMagnitude; Boundary = boundary }

    /// A native two's-complement signed integer that wraps at its boundary.
    let signedInt (name: string) (bits: int) (minValue: string) (maxValue: string) : Representation =
        create name Capability.Native RepresentationFamily.Int bits minValue maxValue Boundary.Wrap

    /// A native unsigned integer that wraps at its boundary.
    let unsignedInt (name: string) (bits: int) (maxValue: string) : Representation =
        create name Capability.Native RepresentationFamily.UInt bits "0" maxValue Boundary.Wrap

    /// A native IEEE 754 binary format: symmetric range, exact boundary (its
    /// overflow is its own infinity).
    let ieee (name: string) (bits: int) (maxFinite: string) : Representation =
        create name Capability.Native RepresentationFamily.Ieee bits (Text.append "-" maxFinite) maxFinite Boundary.Exact

    /// A posit format that saturates at maxpos; capability as declared.
    let posit (name: string) (capability: Capability) (bits: int) (maxPos: string) : Representation =
        create name capability RepresentationFamily.Posit bits (Text.append "-" maxPos) maxPos Boundary.Saturate

    /// The same representation with another capability.
    let withCapability (representation: Representation) (capability: Capability) : Representation =
        { representation with Capability = capability }

/// Constructors for limits.
module Limit =

    /// A named limit.
    let create (name: string) (value: int64) : Limit =
        { Name = name; Value = value }

    /// The verifier's stack ceiling, named as the obligations cite it.
    [<Literal>]
    let StackBytes = "stack-bytes"

    /// The verifier's instruction budget.
    [<Literal>]
    let InstructionBudget = "instruction-budget"

    /// The instruction-set level (BPF v1..v4).
    [<Literal>]
    let IsaLevel = "isa-level"

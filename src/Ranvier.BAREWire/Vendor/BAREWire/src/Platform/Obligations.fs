namespace BAREWire.Platform

open BAREWire.Encoding

/// The decidable shape of an obligation. `Kind` names the form; `A` and `B`
/// are its constants; `Names` and `Values` carry the ranges of a `Disjoint`
/// form as (base, length) pairs in `Values`, one name per pair in `Names`.
type ObligationForm = {
    Kind: string
    A: int64
    B: int64
    Names: string array
    Values: int64 array
}

/// The forms an obligation can take.
[<RequireQualifiedAccess>]
module FormKind =
    /// Shared-buffer facts in Names/Values, decided by linear constraints.
    [<Literal>]
    let SharedBufferLayout: string = "fs9220-layout"
    [<Literal>]
    let SharedBufferExtent: string = "fs9221-extent"
    [<Literal>]
    let SharedBufferSpaceExtent: string = "fs9221-space-extent"
    [<Literal>]
    let SharedBufferLifetime: string = "fs9222-lifetime"
    [<Literal>]
    let SharedBufferExclusivity: string = "fs9223-exclusivity"
    /// A <= B.
    [<Literal>]
    let Leq: string = "leq"
    /// A > 0.
    [<Literal>]
    let Positive: string = "positive"
    /// A is a power of two (and not zero), decided over 64-bit vectors.
    [<Literal>]
    let PowerOfTwo: string = "power-of-two"
    /// For all r, 1 <= r <= A implies r - 1 <= B.
    [<Literal>]
    let RangeMinusOneLeq: string = "range-minus-one-leq"
    /// The (base, length) ranges in `Values` are pairwise disjoint.
    [<Literal>]
    let Disjoint: string = "disjoint"

/// One proof obligation stated against a declaration: its id, its family,
/// the logic it is decided in, the sentence it asserts, the declaration it
/// cites (`<description id>:<declaration name>`), the weakness ids it guards
/// against, and its decidable form.
type Obligation = {
    Id: string
    Kind: string
    Logic: Logic
    Statement: string
    Source: string
    Refs: string array
    Form: ObligationForm
}

/// The obligation families this observer states.
[<RequireQualifiedAccess>]
module ObligationKind =
    [<Literal>]
    let BufferCapacity: string = "buffer-capacity"
    [<Literal>]
    let InputBufferBound: string = "input-buffer-bound"
    [<Literal>]
    let InputCopyBound: string = "input-copy-bound"
    [<Literal>]
    let SpaceAlignment: string = "space-alignment"
    [<Literal>]
    let MemoryMapDisjointness: string = "memory-map-disjointness"
    /// A buffer carried by a transport fits the transport's largest unit.
    [<Literal>]
    let TransportUnit: string = "transport-unit"
    /// A space of kind Stack fits the host's declared stack-bytes limit.
    [<Literal>]
    let SpaceLimit: string = "space-limit"

/// The third observer of the description (docs/11, "Three observers, one
/// truth"): proof obligations stated against the declared facts, so a
/// buffer's capacity or a space's base is cited by name rather than restated
/// as a constant in witness code. `ofDescription` is total and deterministic;
/// `smtLib` renders the refutation form HelloProof checks (`unsat` means the
/// obligation holds); `ledgerLine` is the one-line record for the ledger.
/// This observer and its external ledger are verification scaffolding for the
/// proof-carrying PSG's joint constraint mechanism, not its canonical proof
/// state. Retain the scaffold until cross-layer agreement demonstrates that
/// the graph carries these obligations and their evidence without loss.
module Obligations =

    let private push (acc: Obligation array) (o: Obligation) : Obligation array =
        let n = Array.length acc
        let m = n + 1
        let out : Obligation array = Array.zeroCreate m
        let mutable i = 0
        while i < n do
            Array.set out i (Array.get acc i)
            i <- i + 1
        Array.set out n o
        out

    /// The slug of a name: lower case, every run of characters outside
    /// `a-z0-9` collapsed to one underscore.
    let slug (s: string) : string =
        let bytes = Text.toUtf8 s
        let n = Array.length bytes
        let out : byte array = Array.zeroCreate n
        let mutable o = 0
        let mutable i = 0
        let mutable lastUnderscore = false
        while i < n do
            let b = Array.get bytes i
            let isLower = b >= 97uy && b <= 122uy
            let isUpper = b >= 65uy && b <= 90uy
            let isDigit = b >= 48uy && b <= 57uy
            let keep = isLower || isUpper || isDigit
            let lowered = byte (int b + 32)
            let c = if isUpper then lowered else b
            let underscore = (not keep) && (not lastUnderscore)
            if keep then
                Array.set out o c
                o <- o + 1
            if underscore then
                Array.set out o 95uy
                o <- o + 1
            lastUnderscore <- not keep
            i <- i + 1
        Text.ofUtf8 (Array.sub out 0 o)

    let private source (desc: PlatformDescription) (declaration: string) : string =
        Text.append (Text.append desc.Id ":") declaration

    let private scalar (kind: string) (a: int64) (b: int64) : ObligationForm =
        if kind = FormKind.Leq then
            { Kind = kind; A = a; B = b; Names = [| "lhs"; "rhs" |]; Values = [| a; b |] }
        elif kind = FormKind.Positive then
            { Kind = kind; A = a; B = b; Names = [| "value" |]; Values = [| a |] }
        else
            { Kind = kind; A = a; B = b; Names = Array.zeroCreate 0; Values = Array.zeroCreate 0 }

    let private bufferObligations (acc: Obligation array) (desc: PlatformDescription) (b: BufferSchema) : Obligation array =
        let s = slug b.Name
        let cap = Fmt.ofInt64 b.Capacity
        let space = PlatformDescription.spaceOfBuffer desc b
        let spaceCapacity =
            match space with
            | Some sp -> sp.Capacity
            | None -> 0L
        let spaceText =
            match space with
            | Some sp -> Text.append (Text.append (Text.append "the capacity " (Fmt.ofInt64 sp.Capacity)) " of its space ") sp.Name
            | None -> Text.append (Text.append "the capacity of its space " b.Space) ", which is not declared (taken as 0)"
        let refsCapacity = [| "CWE-131"; "CWE-120" |]
        let positive =
            { Id = Text.append "capacity_positive_" s
              Kind = ObligationKind.BufferCapacity
              Logic = Logic.QfLia
              Statement = Text.append (Text.append (Text.append "buffer " b.Name) " declares a positive capacity (") (Text.append cap " > 0)")
              Source = source desc b.Name
              Refs = refsCapacity
              Form = scalar FormKind.Positive b.Capacity 0L }
        let fits =
            { Id = Text.append "capacity_" s
              Kind = ObligationKind.BufferCapacity
              Logic = Logic.QfLia
              Statement = Text.append (Text.append (Text.append (Text.append "buffer " b.Name) " declares capacity ") cap) (Text.append ", at most " spaceText)
              Source = source desc b.Name
              Refs = refsCapacity
              Form = scalar FormKind.Leq b.Capacity spaceCapacity }
        let acc1 = push (push acc positive) fits
        let trimmed = Framing.hasDelimiter b.Framing && b.TrimDelimiter
        if trimmed then
            let bound =
                { Id = Text.append "input_bound_" s
                  Kind = ObligationKind.InputBufferBound
                  Logic = Logic.QfLia
                  Statement = Text.append (Text.append (Text.append (Text.append "the count handed to the reader of " b.Name) " is its declared capacity (") cap) (Text.append (Text.append "), so the reader writes at most the allocation the same declaration sizes (" cap) ")")
                  Source = source desc b.Name
                  Refs = [| "CWE-120" |]
                  Form = scalar FormKind.Leq b.Capacity b.Capacity }
            let less = Fmt.ofInt64 (b.Capacity - 1L)
            let copy =
                { Id = Text.append "input_copy_bound_" s
                  Kind = ObligationKind.InputCopyBound
                  Logic = Logic.QfLia
                  Statement = Text.append (Text.append (Text.append (Text.append "for any successful read of r bytes into " b.Name) " (1 <= r <= ") cap) (Text.append (Text.append "), the trimmed copy of r - 1 bytes is within " less) " bytes")
                  Source = source desc b.Name
                  Refs = [| "CWE-120"; "CWE-787" |]
                  Form = scalar FormKind.RangeMinusOneLeq b.Capacity (b.Capacity - 1L) }
            push (push acc1 bound) copy
        else acc1

    let private hasBase (s: MemorySpace) : bool =
        let h =
            match s.Base with
            | Some _ -> true
            | None -> false
        h

    let private baseOf (s: MemorySpace) : int64 =
        let b =
            match s.Base with
            | Some v -> v
            | None -> 0L
        b

    let private alignObligation (desc: PlatformDescription) (s: MemorySpace) : Obligation =
        { Id = Text.append "align_" (slug s.Name)
          Kind = ObligationKind.SpaceAlignment
          Logic = Logic.QfBv
          Statement = Text.append (Text.append (Text.append "space " s.Name) " declares alignment ") (Text.append (Fmt.ofInt s.Alignment) ", a power of two")
          Source = source desc s.Name
          Refs = Array.zeroCreate 0
          Form = scalar FormKind.PowerOfTwo (int64 s.Alignment) 0L }

    let private disjointObligation (desc: PlatformDescription) (based: MemorySpace array) : Obligation =
        let n = Array.length based
        let names : string array = Array.zeroCreate n
        let values : int64 array = Array.zeroCreate (2 * n)
        let mutable i = 0
        while i < n do
            let s = Array.get based i
            Array.set names i s.Name
            Array.set values (2 * i) (baseOf s)
            Array.set values (2 * i + 1) s.Capacity
            i <- i + 1
        { Id = "spaces_disjoint"
          Kind = ObligationKind.MemoryMapDisjointness
          Logic = Logic.QfLia
          Statement = Text.append (Text.append (Text.append "the " (Fmt.ofInt n)) " spaces with declared bases (") (Text.append (Fmt.join ", " names) ") occupy pairwise-disjoint address ranges")
          Source = source desc "spaces"
          Refs = [| "CWE-787"; "CWE-125" |]
          Form = { Kind = FormKind.Disjoint; A = int64 n; B = 0L; Names = names; Values = values } }

    /// For a transport with a declared largest unit and a schema, every buffer
    /// of the same schema fits the unit: the frame a wire carries is a fact of
    /// its declaration, not a measurement (ThreeBody's open payload question,
    /// docs/11).
    let private transportObligations (acc: Obligation array) (desc: PlatformDescription) (t: Transport) : Obligation array =
        let mutable out = acc
        if t.MaxUnit > 0L && String.length t.Schema > 0 then
            let nb = Array.length desc.Buffers
            let mutable i = 0
            while i < nb do
                let b = Array.get desc.Buffers i
                if b.Schema = t.Schema then
                    let id = Text.append (Text.append (Text.append "fits_" (slug t.Name)) "_") (slug b.Name)
                    let statement =
                        Text.append (Text.append (Text.append (Text.append "buffer " b.Name) " (") (Fmt.ofInt64 b.Capacity))
                            (Text.append (Text.append (Text.append " bytes) fits one unit of transport " t.Name) " (") (Text.append (Fmt.ofInt64 t.MaxUnit) " bytes)"))
                    out <- push out { Id = id; Kind = ObligationKind.TransportUnit; Logic = Logic.QfLia; Statement = statement
                                      Source = source desc t.Name; Refs = [| "CWE-120" |]; Form = scalar FormKind.Leq b.Capacity t.MaxUnit }
                i <- i + 1
        out

    /// A space of kind Stack fits the host's declared `stack-bytes` limit
    /// when one is declared: the verifier's ceiling as a cited declaration.
    let private limitObligations (acc: Obligation array) (desc: PlatformDescription) : Obligation array =
        let mutable out = acc
        let limit = PlatformDescription.tryFindLimit desc Limit.StackBytes
        let ceiling =
            match limit with
            | Some l -> l.Value
            | None -> 0L
        if ceiling > 0L then
            let ns = Array.length desc.Spaces
            let mutable i = 0
            while i < ns do
                let s = Array.get desc.Spaces i
                if s.Kind = MemoryKind.Stack then
                    let id = Text.append "stack_within_limit_" (slug s.Name)
                    let statement =
                        Text.append (Text.append (Text.append "space " s.Name) " declares capacity ") (Text.append (Text.append (Fmt.ofInt64 s.Capacity) ", at most the stack-bytes limit ") (Fmt.ofInt64 ceiling))
                    out <- push out { Id = id; Kind = ObligationKind.SpaceLimit; Logic = Logic.QfLia; Statement = statement
                                      Source = source desc s.Name; Refs = [| "CWE-121" |]; Form = scalar FormKind.Leq s.Capacity ceiling }
                i <- i + 1
        out

    /// Two names can share a slug ("Console Readln" and "console-readln"); a
    /// repeated id gets `_2`, `_3`, ... so every anchor is unique.
    let private dedupeIds (obs: Obligation array) : Obligation array =
        let n = Array.length obs
        let out : Obligation array = Array.zeroCreate n
        let used (items: Obligation array) (upto: int) (id: string) : bool =
            let mutable j = 0
            let mutable found = false
            while j < upto && not found do
                found <- (Array.get items j).Id = id
                j <- j + 1
            found
        let mutable i = 0
        while i < n do
            let o = Array.get obs i
            let mutable id = o.Id
            if used out i id then
                let mutable suffix = 2
                id <- Text.append (Text.append o.Id "_") (Fmt.ofInt suffix)
                // Reserve original names too: a generated x_2 must not take
                // the id belonging to a declaration encountered later.
                while used out i id || used obs n id do
                    suffix <- suffix + 1
                    id <- Text.append (Text.append o.Id "_") (Fmt.ofInt suffix)
            Array.set out i { o with Id = id }
            i <- i + 1
        out

    /// Every obligation the description supports, in declaration order:
    /// buffers first (capacity, then the input bounds of a trimmed delimited
    /// buffer), then transports (each carried buffer fits the unit), then the
    /// stack limit, then the alignment of each space with a declared base,
    /// then the disjointness of all spaces with declared bases.
    let ofDescription (desc: PlatformDescription) : Obligation array =
        let start : Obligation array = Array.zeroCreate 0
        let nb = Array.length desc.Buffers
        let mutable acc = start
        let mutable i = 0
        while i < nb do
            acc <- bufferObligations acc desc (Array.get desc.Buffers i)
            i <- i + 1
        let nt = Array.length desc.Transports
        let mutable ti = 0
        while ti < nt do
            acc <- transportObligations acc desc (Array.get desc.Transports ti)
            ti <- ti + 1
        acc <- limitObligations acc desc
        let ns = Array.length desc.Spaces
        let mutable basedCount = 0
        let mutable j = 0
        while j < ns do
            let s = Array.get desc.Spaces j
            if hasBase s then
                acc <- push acc (alignObligation desc s)
                basedCount <- basedCount + 1
            j <- j + 1
        let based : MemorySpace array = Array.zeroCreate basedCount
        let mutable k = 0
        let mutable w = 0
        while k < ns do
            let s = Array.get desc.Spaces k
            if hasBase s then
                Array.set based w s
                w <- w + 1
            k <- k + 1
        let all = if basedCount > 0 then push acc (disjointObligation desc based) else acc
        dedupeIds all

    /// Four refutable obligations per placement, preserving all declared input facts.
    /// Every arithmetic, alignment, divisibility, field-geometry and access-mode law
    /// is stated directly in QF_LIA over declared quantities rather than pre-computed host flags.
    let sharedBufferObligations (desc: PlatformDescription) (placements: SharedBufferPlacement array) : Obligation array =
        let flag value = if value then 1L else 0L
        let obs : Obligation array = Array.zeroCreate (placements.Length * 4)
        for i = 0 to placements.Length - 1 do
            let p = placements.[i]
            let s = p.SpaceDescriptor
            let layout = p.StructLayout.Layout
            let space = PlatformDescription.tryFindSpace desc s.Space
            let buffer = PlatformDescription.tryFindSharedBufferSchema desc p
            let spaceCapacity = match space with Some sp -> sp.Capacity | None -> 0L
            let bufferCapacity =
                match buffer with
                | Some b -> b.Capacity
                | None -> if Array.length desc.Buffers = 0 then spaceCapacity else 0L
            let spaceAlign = match space with Some sp -> int64 sp.Alignment | None -> 0L
            let coreByteOrder = match desc.Core with Some core -> SharedBufferPlacement.byteOrderCode core.Endianness | None -> 0L
            let spaceByteOrder = SharedBufferPlacement.byteOrderCode s.ByteOrder
            let layoutByteOrder = SharedBufferPlacement.byteOrderCode p.LayoutByteOrder
            let spaceKind = match space with Some sp -> SharedBufferPlacement.memoryKindCode sp.Kind | None -> 0L
            let memoryRegion = SharedBufferPlacement.memoryRegionCode s.MemoryRegion
            let spaceAccess = match space with Some sp -> SharedBufferPlacement.platformAccessCode sp.Access | None -> 0L
            let bufferAccess =
                match buffer with
                | Some b -> SharedBufferPlacement.platformAccessCode b.Access
                | None -> if Array.length desc.Buffers = 0 then spaceAccess else 0L
            let fieldCount = layout.Fields.Length
            let make prefix kind formA formB code statement refs names values =
                { Id = Text.append prefix (slug p.Name); Kind = code; Logic = Logic.QfLia
                  Statement = statement; Source = source desc p.Name; Refs = refs
                  Form = { Kind = kind; A = formA; B = formB; Names = names; Values = values } }
            obs.[4 * i] <-
                make "fs9221_extent_" FormKind.SharedBufferExtent (int64 p.MeasuredElementSize) 0L FindingKind.SharedBufferExtentInvalid
                    "FS9221: positive bounded extent and in-range element access" [| "CWE-125"; "CWE-787" |]
                    [| "elem_size"; "measured_size"; "elem_count"; "region_bytes"; "space_cap"; "buffer_cap"; "max_elem_idx"; "required_bytes"; "space_offset" |]
                    [| int64 s.ElementSize; int64 p.MeasuredElementSize; int64 s.ElementCount; int64 s.RegionByteExtent; spaceCapacity
                       bufferCapacity; int64 p.MaxAccessedElementIndex; int64 s.ElementCount * int64 p.MeasuredElementSize; p.SpaceOffset |]
            let baseLayoutNames =
                [| "placement_name_len"; "elem_size"; "measured_size"; "struct_bytes"; "layout_align"; "struct_align"
                   "view_align"; "region_align"; "space_align"; "space_offset"; "view_elem_width"; "view_schema_len"
                   "buffer_schema_len"; "core_byte_order"; "space_byte_order"; "layout_byte_order"; "space_kind"
                   "memory_region"; "raw_ptr_field_count"; "flex_array_count"; "unverified_export_count"
                   "duplicate_name_count"; "field_count" |]
            let baseLayoutValues =
                [| int64 (SharedBufferPlacement.trimmedLength p.Name)
                   int64 s.ElementSize
                   int64 p.MeasuredElementSize
                   int64 layout.Size
                   int64 p.LayoutAlignment
                   int64 layout.Alignment
                   int64 p.ViewLayout.Alignment
                   int64 s.RegionAlignment
                   spaceAlign
                   p.SpaceOffset
                   int64 (SharedBufferPlacement.viewElementWidth p.ViewLayout.Element)
                   int64 (SharedBufferPlacement.trimmedLength p.ViewLayout.Schema)
                   int64 (PlatformDescription.matchedBufferSchemaLength desc p)
                   coreByteOrder
                   spaceByteOrder
                   layoutByteOrder
                   spaceKind
                   memoryRegion
                   int64 (SharedBufferPlacement.rawPointerFieldCount p)
                   flag (SharedBufferPlacement.structHasFlexibleArray p.StructLayout)
                   flag (not p.HasVerifiedExports)
                   int64 (SharedBufferPlacement.priorNameCount placements i)
                   int64 fieldCount |]
            let totalBitFields =
                let mutable count = 0
                for fk = 0 to fieldCount - 1 do
                    count <- count + layout.Fields.[fk].BitFields.Length
                count
            let layoutVarCount = baseLayoutNames.Length + 6 * fieldCount + 5 * totalBitFields
            let layoutNames : string array = Array.zeroCreate layoutVarCount
            let layoutValues : int64 array = Array.zeroCreate layoutVarCount
            for bi = 0 to baseLayoutNames.Length - 1 do
                layoutNames.[bi] <- baseLayoutNames.[bi]
                layoutValues.[bi] <- baseLayoutValues.[bi]
            let mutable wIdx = baseLayoutNames.Length
            for fk = 0 to fieldCount - 1 do
                let fld = layout.Fields.[fk]
                let kText = Fmt.ofInt fk
                let width = int64 (SharedBufferPlacement.viewElementWidth fld.Repr)
                layoutNames.[wIdx] <- Text.append "field_name_len_" kText
                layoutNames.[wIdx + 1] <- Text.append "field_dup_name_" kText
                layoutNames.[wIdx + 2] <- Text.append "field_off_" kText
                layoutNames.[wIdx + 3] <- Text.append "field_elem_count_" kText
                layoutNames.[wIdx + 4] <- Text.append "field_len_" kText
                layoutNames.[wIdx + 5] <- Text.append "field_align_" kText
                layoutValues.[wIdx] <- int64 (SharedBufferPlacement.trimmedLength fld.Name)
                layoutValues.[wIdx + 1] <- int64 (SharedBufferPlacement.priorFieldNameCount layout.Fields fk)
                layoutValues.[wIdx + 2] <- int64 fld.Offset
                layoutValues.[wIdx + 3] <- int64 fld.Count
                layoutValues.[wIdx + 4] <- int64 fld.Count * width
                layoutValues.[wIdx + 5] <- width
                wIdx <- wIdx + 6
                for b = 0 to fld.BitFields.Length - 1 do
                    let bf = fld.BitFields.[b]
                    let kbText = Text.append (Text.append kText "_") (Fmt.ofInt b)
                    layoutNames.[wIdx] <- Text.append "bf_name_len_" kbText
                    layoutNames.[wIdx + 1] <- Text.append "bf_dup_name_" kbText
                    layoutNames.[wIdx + 2] <- Text.append "bf_pos_" kbText
                    layoutNames.[wIdx + 3] <- Text.append "bf_width_" kbText
                    layoutNames.[wIdx + 4] <- Text.append "bf_access_" kbText
                    layoutValues.[wIdx] <- int64 (SharedBufferPlacement.trimmedLength bf.Name)
                    layoutValues.[wIdx + 1] <- int64 (SharedBufferPlacement.priorBitFieldNameCount fld.BitFields b)
                    layoutValues.[wIdx + 2] <- int64 bf.Position
                    layoutValues.[wIdx + 3] <- int64 bf.Width
                    layoutValues.[wIdx + 4] <- SharedBufferPlacement.hardwareAccessCode bf.Access
                    wIdx <- wIdx + 5
            obs.[4 * i + 1] <-
                make "fs9220_layout_" FormKind.SharedBufferLayout (int64 fieldCount) (flag p.StructLayout.MayAlias) FindingKind.SharedBufferLayoutInvalid
                    "FS9220: verified pointer-free layout, alignment, byte order and named space" [| "CWE-704" |]
                    layoutNames layoutValues
            obs.[4 * i + 2] <-
                make "fs9222_lifetime_" FormKind.SharedBufferLifetime 0L 0L FindingKind.SharedBufferLifetimeInvalid
                    "FS9222: region lifetime covers retention and the mapped interval covers all uses" [| "CWE-416"; "CWE-772" |]
                    [| "region_depth"; "lib_retention_depth"; "mapped_depth"; "first_use_depth"; "last_use_depth"; "lib_retention_end_depth"; "unmap_depth"; "leaked" |]
                    [| int64 s.RegionLifetimeDepth; int64 s.LibraryRetentionDepth; int64 p.MappedDepth
                       int64 p.FirstUseDepth; int64 p.LastUseDepth; int64 p.LibraryRetentionEndDepth; int64 p.UnmapDepth; flag p.LeakedAtExit |]
            let baseExclNames =
                [| "declared_access"; "view_access"; "space_access"; "buffer_access"; "lib_mutates"; "concurrent_clef_write"; "overlap_mutable_share" |]
            let baseExclValues =
                [| SharedBufferPlacement.declaredAccessCode s.Access
                   SharedBufferPlacement.hardwareAccessCode p.ViewLayout.Access
                   spaceAccess
                   bufferAccess
                   flag p.LibraryMutatesBuffer
                   flag p.ConcurrentClefMutation
                   flag p.OverlappingMutableShare |]
            let exclNames : string array = Array.zeroCreate (baseExclNames.Length + fieldCount)
            let exclValues : int64 array = Array.zeroCreate (baseExclValues.Length + fieldCount)
            for bi = 0 to baseExclNames.Length - 1 do
                exclNames.[bi] <- baseExclNames.[bi]
                exclValues.[bi] <- baseExclValues.[bi]
            for fk = 0 to fieldCount - 1 do
                let fld = layout.Fields.[fk]
                let idx = baseExclNames.Length + fk
                exclNames.[idx] <- Text.append "field_access_" (Fmt.ofInt fk)
                exclValues.[idx] <- SharedBufferPlacement.hardwareAccessCode fld.Access
            obs.[4 * i + 3] <-
                make "fs9223_exclusivity_" FormKind.SharedBufferExclusivity (int64 fieldCount) 0L FindingKind.SharedBufferConflictingAccess
                    "FS9223: declared access excludes conflicting writers" [| "CWE-362" |]
                    exclNames exclValues
        let mutable all = obs
        for sp in desc.Spaces do
            let placed = placements |> Array.filter (fun p -> p.SpaceDescriptor.Space = sp.Name)
            if placed.Length > 0 then
                let values = placed |> Array.collect (fun p -> [| p.SpaceOffset; int64 p.SpaceDescriptor.RegionByteExtent |])
                all <- push all
                    { Id = Text.append "fs9221_space_" (slug sp.Name)
                      Kind = FindingKind.SharedBufferExtentInvalid; Logic = Logic.QfLia
                      Statement = "FS9221: shared-buffer intervals fit space capacity and are pairwise disjoint"
                      Source = source desc sp.Name; Refs = [| "CWE-125"; "CWE-787" |]
                      Form = { Kind = FormKind.SharedBufferSpaceExtent; A = sp.Capacity; B = 0L
                               Names = placed |> Array.map (fun p -> p.Name); Values = values } }
        dedupeIds all

    /// An SMT-LIB integer numeral; negatives are written `(- n)`.
    let private smtInt (v: int64) : string =
        if v < 0L then Text.append (Text.append "(- " (Fmt.magnitudeText v)) ")" else Fmt.ofInt64 v

    /// A 64-bit SMT-LIB vector literal, `#x` and sixteen hex digits.
    let private hex16 (v: uint64) : string =
        let buf : byte array = Array.zeroCreate 16
        let mutable x = v
        let mutable pos = 15
        while pos >= 0 do
            let d = int (x &&& 15UL)
            let c = if d < 10 then 48 + d else 87 + d
            Array.set buf pos (byte c)
            x <- x >>> 4
            pos <- pos - 1
        Text.append "#x" (Text.ofUtf8 buf)

    let private line (acc: string) (text: string) : string =
        Text.append (Text.append acc text) "\n"

    let private paren2 (op: string) (a: string) (b: string) : string =
        Text.append (Text.append (Text.append (Text.append (Text.append "(" op) " ") a) (Text.append " " b)) ")"

    let private assert' (term: string) : string =
        Text.append (Text.append "(assert " term) ")"

    /// The disjointness term for ranges i and j: one ends before the other begins.
    let private disjointPair (values: int64 array) (i: int) (j: int) : string =
        // Names remain in Form.Names in declaration order. Solver symbols use
        // that ordinal, so distinct declarations never alias through slugging.
        let bi = Text.append "b_" (Fmt.ofInt i)
        let bj = Text.append "b_" (Fmt.ofInt j)
        let li = smtInt (Array.get values (2 * i + 1))
        let lj = smtInt (Array.get values (2 * j + 1))
        paren2 "or" (paren2 "<=" (paren2 "+" bi li) bj) (paren2 "<=" (paren2 "+" bj lj) bi)

    /// 31 positive powers of two (2^0 .. 2^30) for linear QF_LIA alignment and divisibility laws.
    let private pow2Constants : int64 array =
        Array.init 31 (fun k -> 1L <<< k)

    /// Scalar element widths (1, 2, 4, 8 bytes) for Hardware.Repr scalar elements.
    let private elemWidthConstants : int64 array =
        [| 1L; 2L; 4L; 8L |]

    /// QF_LIA disjunction asserting `varName` is a positive 32-bit power of two.
    let private pow2Disjunction (varName: string) : string =
        let branches =
            pow2Constants
            |> Array.map (fun c -> paren2 "=" varName (smtInt c))
        Text.append (Text.append "(or " (Fmt.join " " branches)) ")"

    /// QF_LIA disjunction asserting `divisor` belongs to `constants` and divides both `dividend1` and `dividend2`.
    let private dividesBothFrom (constants: int64 array) (dividend1: string) (dividend2: string) (divisor: string) : string =
        let branches =
            constants
            |> Array.map (fun c ->
                let cText = smtInt c
                let eqDiv = paren2 "=" divisor cText
                let mod1 = paren2 "=" (paren2 "mod" dividend1 cText) "0"
                let mod2 = paren2 "=" (paren2 "mod" dividend2 cText) "0"
                Text.append (Text.append "(and " (Fmt.join " " [| eqDiv; mod1; mod2 |])) ")")
        Text.append (Text.append "(or " (Fmt.join " " branches)) ")"

    /// QF_LIA disjunction asserting scalar field element width `fAlign` belongs to `constants`,
    /// `fOff` is a multiple of `fAlign`, and `fLen = fCount * fAlign`.
    let private fieldGeometryFrom (constants: int64 array) (fOff: string) (fCount: string) (fLen: string) (fAlign: string) : string =
        let branches =
            constants
            |> Array.map (fun c ->
                let cText = smtInt c
                let eqAlign = paren2 "=" fAlign cText
                let modOff = paren2 "=" (paren2 "mod" fOff cText) "0"
                let eqLen = paren2 "=" fLen (paren2 "*" fCount cText)
                Text.append (Text.append "(and " (Fmt.join " " [| eqAlign; modOff; eqLen |])) ")")
        Text.append (Text.append "(or " (Fmt.join " " branches)) ")"

    /// The individual QF_LIA/QF_BV conjuncts C_0 .. C_{m-1} whose conjunction defines `o.Id`.
    /// Every conjunct is independently falsifiable (`SAT(not C_i)`) when value pins are stripped.
    let conclusionConjuncts (o: Obligation) : string array =
        let f = o.Form
        if f.Kind = FormKind.Leq then
            [| "(<= lhs rhs)" |]
        elif f.Kind = FormKind.Positive then
            [| "(> value 0)" |]
        elif f.Kind = FormKind.RangeMinusOneLeq then
            [| "(<= (- r 1) bound)" |]
        elif f.Kind = FormKind.PowerOfTwo then
            let zero = hex16 (uint64 0)
            let one = hex16 (uint64 1)
            let nonzero = Text.append (Text.append "(not " (paren2 "=" "a" zero)) ")"
            let lowbit = paren2 "=" (paren2 "bvand" "a" (paren2 "bvsub" "a" one)) zero
            [| nonzero; lowbit |]
        elif f.Kind = FormKind.Disjoint then
            let n = Array.length f.Names
            let pairCount = n * (n - 1) / 2
            let pairs : string array = Array.zeroCreate pairCount
            let mutable p = 0
            let mutable a = 0
            while a < n do
                let mutable b = a + 1
                while b < n do
                    Array.set pairs p (disjointPair f.Values a b)
                    p <- p + 1
                    b <- b + 1
                a <- a + 1
            pairs
        elif f.Kind = FormKind.SharedBufferSpaceExtent then
            let offsets = Array.init f.Names.Length (fun i -> Text.append "off_" (Fmt.ofInt i))
            let lengths = Array.init f.Names.Length (fun i -> Text.append "len_" (Fmt.ofInt i))
            let mutable terms = []
            for i = 0 to f.Names.Length - 1 do
                let off, len = offsets.[i], lengths.[i]
                terms <- paren2 ">=" off "0" :: terms
                terms <- paren2 ">" len "0" :: terms
                terms <- paren2 "<=" (paren2 "+" off len) "space_cap" :: terms
            let total =
                if lengths.Length = 1 then lengths.[0]
                else Text.append (Text.append "(+ " (Fmt.join " " lengths)) ")"
            terms <- paren2 "<=" total "space_cap" :: terms
            for i = 0 to f.Names.Length - 2 do
                for j = i + 1 to f.Names.Length - 1 do
                    terms <-
                        (paren2 "or" (paren2 "<=" (paren2 "+" offsets.[i] lengths.[i]) offsets.[j])
                                     (paren2 "<=" (paren2 "+" offsets.[j] lengths.[j]) offsets.[i])) :: terms
            terms |> List.rev |> List.toArray
        elif f.Kind = FormKind.SharedBufferExtent then
            let stride = smtInt f.A
            [| "(> elem_size 0)"
               "(> measured_size 0)"
               Text.append (Text.append "(= measured_size " stride) ")"
               "(= elem_size measured_size)"
               "(> elem_count 0)"
               "(> region_bytes 0)"
               "(> space_cap 0)"
               "(> buffer_cap 0)"
               Text.append (Text.append "(= required_bytes (* elem_count " stride) "))"
               "(<= required_bytes 2147483647)"
               "(>= region_bytes required_bytes)"
               "(<= region_bytes space_cap)"
               "(<= region_bytes buffer_cap)"
               "(>= space_offset 0)"
               "(<= (+ space_offset region_bytes) space_cap)"
               "(>= max_elem_idx 0)"
               "(< max_elem_idx elem_count)" |]
        elif f.Kind = FormKind.SharedBufferLayout then
            let fieldCount = if f.A > 0L then int f.A else 0
            let mayAlias = f.B <> 0L
            let baseTerms =
                [| "(> placement_name_len 0)"
                   "(> measured_size 0)"
                   "(> struct_bytes 0)"
                   "(= elem_size measured_size)"
                   "(= struct_bytes measured_size)"
                   "(= struct_align layout_align)"
                   "(= view_align layout_align)"
                   "(>= region_align layout_align)"
                   "(<= region_align space_align)"
                   "(>= space_offset 0)"
                   pow2Disjunction "space_align"
                   dividesBothFrom pow2Constants "measured_size" "region_align" "layout_align"
                   dividesBothFrom pow2Constants "space_align" "space_offset" "region_align"
                   "(> view_schema_len 0)"
                   "(> buffer_schema_len 0)"
                   "(>= measured_size view_elem_width)"
                   "(>= view_align view_elem_width)"
                   "(>= layout_align view_elem_width)"
                   dividesBothFrom elemWidthConstants "measured_size" "view_align" "view_elem_width"
                   "(or (= core_byte_order 1) (= core_byte_order 2))"
                   "(= space_byte_order core_byte_order)"
                   "(= layout_byte_order space_byte_order)"
                   "(or (and (= space_kind 1) (= memory_region 1)) (and (= space_kind 2) (= memory_region 2)) (and (= space_kind 3) (= memory_region 3)) (and (= space_kind 4) (= memory_region 4)) (and (= space_kind 5) (or (= memory_region 1) (= memory_region 2))))"
                   "(= raw_ptr_field_count 0)"
                   "(= flex_array_count 0)"
                   "(= unverified_export_count 0)"
                   "(= duplicate_name_count 0)"
                   "(> field_count 0)" |]
            let mutable extra = []
            for fk = 0 to fieldCount - 1 do
                let kText = Fmt.ofInt fk
                let fNameLen = Text.append "field_name_len_" kText
                let fDupName = Text.append "field_dup_name_" kText
                let fOff = Text.append "field_off_" kText
                let fCount = Text.append "field_elem_count_" kText
                let fLen = Text.append "field_len_" kText
                let fAlign = Text.append "field_align_" kText
                extra <- paren2 ">" fNameLen "0" :: extra
                extra <- paren2 "=" fDupName "0" :: extra
                extra <- paren2 ">=" fOff "0" :: extra
                extra <- paren2 ">" fCount "0" :: extra
                extra <- paren2 ">" fLen "0" :: extra
                extra <- paren2 "<=" (paren2 "+" fOff fLen) "measured_size" :: extra
                extra <- paren2 ">=" "layout_align" fAlign :: extra
                extra <- fieldGeometryFrom elemWidthConstants fOff fCount fLen fAlign :: extra
                let mutable bfCount = 0
                while Array.contains (Text.append (Text.append (Text.append "bf_pos_" kText) "_") (Fmt.ofInt bfCount)) f.Names do
                    bfCount <- bfCount + 1
                for b = 0 to bfCount - 1 do
                    let kbText = Text.append (Text.append kText "_") (Fmt.ofInt b)
                    let bfNameLen = Text.append "bf_name_len_" kbText
                    let bfDupName = Text.append "bf_dup_name_" kbText
                    let bfPos = Text.append "bf_pos_" kbText
                    let bfWidth = Text.append "bf_width_" kbText
                    let bfAccess = Text.append "bf_access_" kbText
                    extra <- paren2 ">" bfNameLen "0" :: extra
                    extra <- paren2 "=" bfDupName "0" :: extra
                    extra <- paren2 ">=" bfPos "0" :: extra
                    extra <- paren2 ">" bfWidth "0" :: extra
                    extra <- paren2 "<=" (paren2 "+" bfPos bfWidth) (paren2 "*" fAlign "8") :: extra
                    extra <- paren2 ">=" bfAccess "1" :: extra
                    extra <- paren2 "<=" bfAccess "3" :: extra
                if bfCount >= 2 then
                    for ba = 0 to bfCount - 2 do
                        let kaText = Text.append (Text.append kText "_") (Fmt.ofInt ba)
                        let posA = Text.append "bf_pos_" kaText
                        let widA = Text.append "bf_width_" kaText
                        for bb = ba + 1 to bfCount - 1 do
                            let kbText = Text.append (Text.append kText "_") (Fmt.ofInt bb)
                            let posB = Text.append "bf_pos_" kbText
                            let widB = Text.append "bf_width_" kbText
                            let bfDisj =
                                paren2 "or"
                                    (paren2 "<=" (paren2 "+" posA widA) posB)
                                    (paren2 "<=" (paren2 "+" posB widB) posA)
                            extra <- bfDisj :: extra
            if not mayAlias && fieldCount >= 2 then
                for fi = 0 to fieldCount - 2 do
                    let iText = Fmt.ofInt fi
                    let offI = Text.append "field_off_" iText
                    let lenI = Text.append "field_len_" iText
                    for fj = fi + 1 to fieldCount - 1 do
                        let jText = Fmt.ofInt fj
                        let offJ = Text.append "field_off_" jText
                        let lenJ = Text.append "field_len_" jText
                        let disj =
                            paren2 "or"
                                (paren2 "<=" (paren2 "+" offI lenI) offJ)
                                (paren2 "<=" (paren2 "+" offJ lenJ) offI)
                        extra <- disj :: extra
            Array.append baseTerms (extra |> List.rev |> List.toArray)
        elif f.Kind = FormKind.SharedBufferLifetime then
            [| "(> region_depth 0)"
               "(>= lib_retention_depth 0)"
               "(<= region_depth lib_retention_depth)"
               "(>= mapped_depth 0)"
               "(<= mapped_depth first_use_depth)"
               "(<= first_use_depth last_use_depth)"
               "(<= last_use_depth lib_retention_end_depth)"
               "(< lib_retention_end_depth unmap_depth)"
               "(= leaked 0)" |]
        elif f.Kind = FormKind.SharedBufferExclusivity then
            let fieldCount = if f.A > 0L then int f.A else 0
            let baseTerms =
                [| "(or (and (= declared_access 1) (= view_access 1)) (and (= declared_access 2) (= view_access 2)) (and (= declared_access 3) (= view_access 3)) (and (= declared_access 4) (= view_access 3)))"
                   "(or (and (= declared_access 1) (or (= space_access 1) (= space_access 3))) (and (= declared_access 2) (or (= space_access 2) (= space_access 3))) (and (or (= declared_access 3) (= declared_access 4)) (= space_access 3)))"
                   "(or (and (= declared_access 1) (or (= buffer_access 1) (= buffer_access 3))) (and (= declared_access 2) (or (= buffer_access 2) (= buffer_access 3))) (and (or (= declared_access 3) (= declared_access 4)) (= buffer_access 3)))"
                   "(or (= lib_mutates 0) (= lib_mutates 1))"
                   "(not (and (= declared_access 1) (= lib_mutates 1)))"
                   "(= concurrent_clef_write 0)"
                   "(= overlap_mutable_share 0)" |]
            let fieldTerms =
                Array.init fieldCount (fun fk ->
                    let fa = Text.append "field_access_" (Fmt.ofInt fk)
                    let roCase = paren2 "and" "(= declared_access 1)" (paren2 "=" fa "1")
                    let woCase = paren2 "and" "(= declared_access 2)" (paren2 "=" fa "2")
                    let rwCase =
                        paren2 "and"
                            "(or (= declared_access 3) (= declared_access 4))"
                            (paren2 "or" (paren2 "=" fa "1") (paren2 "=" fa "3"))
                    Text.append (Text.append "(or " (Fmt.join " " [| roCase; woCase; rwCase |])) ")")
            Array.append baseTerms fieldTerms
        else
            [| "false" |]

    let private conjunctionOf (conjuncts: string array) : string =
        if conjuncts.Length = 0 then "true"
        elif conjuncts.Length = 1 then conjuncts.[0]
        else Text.append (Text.append "(and " (Fmt.join " " conjuncts)) ")"

    let private emitDeclarationsAndPins (includePins: bool) (h4: string) (f: ObligationForm) : string =
        if f.Kind = FormKind.Leq then
            let d1 = line h4 "(declare-const lhs Int)"
            let d2 = line d1 "(declare-const rhs Int)"
            if includePins then
                let d3 = line d2 (assert' (paren2 "=" "lhs" (smtInt f.A)))
                line d3 (assert' (paren2 "=" "rhs" (smtInt f.B)))
            else d2
        elif f.Kind = FormKind.Positive then
            let d1 = line h4 "(declare-const value Int)"
            if includePins then
                line d1 (assert' (paren2 "=" "value" (smtInt f.A)))
            else d1
        elif f.Kind = FormKind.RangeMinusOneLeq then
            let d1 = line h4 "(declare-const r Int)"
            let d2 = line d1 "(declare-const bound Int)"
            if includePins then
                let d3 = line d2 "(assert (>= r 1))"
                let d4 = line d3 (assert' (paren2 "<=" "r" (smtInt f.A)))
                line d4 (assert' (paren2 "=" "bound" (smtInt f.B)))
            else d2
        elif f.Kind = FormKind.PowerOfTwo then
            let d1 = line h4 "(declare-const a (_ BitVec 64))"
            if includePins then
                line d1 (assert' (paren2 "=" "a" (hex16 (uint64 f.A))))
            else d1
        elif f.Kind = FormKind.Disjoint then
            let n = Array.length f.Names
            let mutable d = h4
            let mutable i = 0
            while i < n do
                let name = Text.append "b_" (Fmt.ofInt i)
                d <- line d (Text.append (Text.append "(declare-const " name) " Int)")
                if includePins then
                    d <- line d (assert' (paren2 "=" name (smtInt (Array.get f.Values (2 * i)))))
                i <- i + 1
            d
        elif f.Kind = FormKind.SharedBufferSpaceExtent then
            let mutable d = line h4 "(declare-const space_cap Int)"
            if includePins then
                d <- line d (assert' (paren2 "=" "space_cap" (smtInt f.A)))
            for i = 0 to f.Names.Length - 1 do
                let off = Text.append "off_" (Fmt.ofInt i)
                let len = Text.append "len_" (Fmt.ofInt i)
                d <- line d (Text.append (Text.append "(declare-const " off) " Int)")
                d <- line d (Text.append (Text.append "(declare-const " len) " Int)")
                if includePins then
                    d <- line d (assert' (paren2 "=" off (smtInt f.Values.[2 * i])))
                    d <- line d (assert' (paren2 "=" len (smtInt f.Values.[2 * i + 1])))
            d
        elif f.Kind = FormKind.SharedBufferExtent || f.Kind = FormKind.SharedBufferLayout
             || f.Kind = FormKind.SharedBufferLifetime || f.Kind = FormKind.SharedBufferExclusivity then
            let mutable d = h4
            for i = 0 to f.Names.Length - 1 do
                let name = f.Names.[i]
                d <- line d (Text.append (Text.append "(declare-const " name) " Int)")
                if includePins then
                    d <- line d (assert' (paren2 "=" name (smtInt f.Values.[i])))
            d
        else h4

    /// The scaffold's SMT-LIB serialization of one obligation: the definition is
    /// asserted equal to a Boolean named by the id, its negation is asserted,
    /// and `unsat` means the obligation holds. Integer forms are stated in
    /// QF_LIA; `PowerOfTwo` in QF_BV over 64-bit vectors.
    let smtLib (o: Obligation) : string =
        let f = o.Form
        let h1 = line "" (Text.append (Text.append (Text.append "; " o.Id) ": ") o.Statement)
        let h2 = line h1 (Text.append "; origin: " o.Source)
        let h3 = line h2 (Text.append (Text.append "(set-logic " o.Logic) ")")
        let h4 = line h3 (Text.append (Text.append "(declare-const " o.Id) " Bool)")
        let d = emitDeclarationsAndPins true h4 f
        let term = conjunctionOf (conclusionConjuncts o)
        let body = line d (assert' (paren2 "=" o.Id term))
        let t1 = line body (Text.append (Text.append "(assert (not " o.Id) "))")
        let t2 = line t1 "(check-sat)"
        line t2 "(reset)"

    /// Premise satisfiability query (`SAT(H)`): declares symbols, asserts value pins H,
    /// and asserts `(= <id> <C>)` without `(assert (not <id>))`. `sat` confirms H is consistent.
    let smtLibPremises (o: Obligation) : string =
        let f = o.Form
        let h1 = line "" (Text.append (Text.append (Text.append "; " o.Id) " premises: ") o.Statement)
        let h2 = line h1 (Text.append "; origin: " o.Source)
        let h3 = line h2 (Text.append (Text.append "(set-logic " o.Logic) ")")
        let h4 = line h3 (Text.append (Text.append "(declare-const " o.Id) " Bool)")
        let d = emitDeclarationsAndPins true h4 f
        let term = conjunctionOf (conclusionConjuncts o)
        let body = line d (assert' (paren2 "=" o.Id term))
        let t1 = line body "(check-sat)"
        line t1 "(reset)"

    /// Unpinned conclusion non-vacuity query (`SAT(not C)`): declares all participant symbols
    /// without value-pinning equalities H, defines `(= <id> <C>)`, and asserts `(not <id>)`.
    /// `sat` confirms the conclusion C is falsifiable on its own.
    let smtLibUnpinned (o: Obligation) : string =
        let f = o.Form
        let h1 = line "" (Text.append (Text.append (Text.append "; " o.Id) " non-vacuity: ") o.Statement)
        let h2 = line h1 (Text.append "; origin: " o.Source)
        let h3 = line h2 (Text.append (Text.append "(set-logic " o.Logic) ")")
        let h4 = line h3 (Text.append (Text.append "(declare-const " o.Id) " Bool)")
        let d = emitDeclarationsAndPins false h4 f
        let term = conjunctionOf (conclusionConjuncts o)
        let body = line d (assert' (paren2 "=" o.Id term))
        let t1 = line body (Text.append (Text.append "(assert (not " o.Id) "))")
        let t2 = line t1 "(check-sat)"
        line t2 "(reset)"

    /// Per-conjunct non-vacuity queries (`SAT(not C_i)` for each conjunct `C_i` of `conclusionConjuncts o`):
    /// strips value pins H and checks that every individual conjunct is independently falsifiable (`sat`).
    let smtLibClauseNonVacuity (o: Obligation) : string array =
        let f = o.Form
        let conjuncts = conclusionConjuncts o
        conjuncts
        |> Array.mapi (fun idx clause ->
            let h1 = line "" (Text.append (Text.append (Text.append "; " o.Id) " clause ") (Fmt.ofInt idx))
            let h2 = line h1 (Text.append "; origin: " o.Source)
            let h3 = line h2 (Text.append (Text.append "(set-logic " o.Logic) ")")
            let h4 = line h3 (Text.append (Text.append "(declare-const " o.Id) " Bool)")
            let d = emitDeclarationsAndPins false h4 f
            let body = line d (assert' (paren2 "=" o.Id clause))
            let t1 = line body (Text.append (Text.append "(assert (not " o.Id) "))")
            let t2 = line t1 "(check-sat)"
            line t2 "(reset)")

    /// One line for the obligation ledger:
    /// `obligation <id> kind=<k> logic=<l> source=<s> refs=<a,b> : <statement>`.
    let ledgerLine (o: Obligation) : string =
        let refs = if Array.length o.Refs = 0 then "none" else Fmt.join "," o.Refs
        let l1 = Text.append (Text.append "obligation " o.Id) (Text.append " kind=" o.Kind)
        let l2 = Text.append l1 (Text.append " logic=" o.Logic)
        let l3 = Text.append l2 (Text.append " source=" o.Source)
        let l4 = Text.append l3 (Text.append " refs=" refs)
        Text.append l4 (Text.append " : " o.Statement)

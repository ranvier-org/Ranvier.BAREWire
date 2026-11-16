namespace BAREWire.Descriptors

/// Shared-buffer space and hardware region kind for Farscape M10 bindings
/// and BAREWire.Platform. Access is ReadOnly, WriteOnly, ReadWrite, or
/// ExclusiveMutable; byte order, element geometry and lifetime/retention
/// depths describe the library's declared sharing contract.
type MemorySpaceDescriptor = {
    Space: string
    MemoryRegion: BAREWire.Hardware.MemoryRegionKind
    Access: string
    ByteOrder: string
    ElementSize: int
    ElementCount: int
    RegionByteExtent: int
    RegionAlignment: int
    RegionLifetimeDepth: int
    LibraryRetentionDepth: int
}

/// Binding descriptors (clef-lang-spec platform-bindings.md, "Layer 2: Binding
/// Libraries"): the declaration a generator (Farscape) emits beside every
/// extern, carrying the C ABI representation of each parameter and of the
/// return as data, in a quotation the compiler reads structurally by type name
/// and field name and never evaluates (expressions.md, "Quoted Expressions").
///
/// The width of a C integer lives here, never in the Clef signature: the
/// signature spells `int` and the descriptor says `Integer (Signed, 32)`
/// (Dimensional_Range_Design.md, "The Farscape leg"). A descriptor is a
/// declaration: a binding whose value is `Expr<FunctionDescriptor>` is never
/// initialised or emitted, so the closed vocabularies here are unions, the
/// spec's own spelling (`CDecl | StdCall | FastCall`), not string tags; the
/// import wall docs/12 `record-union-field` names is a lowering concern, and
/// a declaration is never lowered.

/// Whether an integer representation carries a sign.
type Signedness =
    | Signed
    | Unsigned

/// A parameter's or return's representation at the C ABI: the family and its
/// bits as data. `Named` cites a descriptor by its binding name (a struct, an
/// opaque handle) and carries no width of its own.
type TypeRef =
    | Integer of Signedness * int
    | Float of int
    | Pointer of int
    | Bool
    | Void
    | Named of string
    | Entry of string

/// An argument the binding supplies at every call instead of the caller
/// (Farscape #192). Its Clef signature has no parameter for it.
type FixedArgument =
    /// A function-pointer sentinel the library defines as a cast integer and
    /// gives a meaning of its own, never an entry: SQLite's SQLITE_TRANSIENT,
    /// `((sqlite3_destructor_type)-1)`, under which SQLite copies the bound
    /// value before the call returns. The macro that names it and the integer
    /// clang evaluates for it; the parameter's `Type` is its representation.
    | EntrySentinel of name: string * value: int

/// How an argument crosses: by value, or by reference to the caller's storage.
type PassBy =
    | Value
    | Reference
    /// The callee may read the referenced storage but must not mutate it.
    | ReadOnlyReference
    /// The binding passes this constant; no Clef argument crosses.
    | Fixed of FixedArgument

/// The calling convention the C side expects.
type CallConv =
    | CDecl
    | StdCall
    | FastCall

/// Who owns what the call returns or is handed.
type Transfer =
    | CallerOwns
    | CalleeOwns
    | Borrowed
    /// No ownership hypothesis was supplied. This grants no lifetime or transfer authority.
    | Undeclared

/// One parameter: its C name, its representation, and how it is passed.
type ParameterInfo = {
    Name: string
    Type: TypeRef
    PassBy: PassBy
}

/// The descriptor of one extern (the spec's `FunctionDescriptor`, its field
/// names exactly): the C symbol, its parameters in order, its return, its
/// calling convention and the ownership transfer of its result.
type FunctionDescriptor = {
    CName: string
    Parameters: ParameterInfo array
    ReturnType: TypeRef
    CallingConvention: CallConv
    OwnershipTransfer: Transfer
}

/// A foreign result bound is constant or relative to a zero-based native
/// argument ordinal. Each call instantiates the latter with its actual argument;
/// the ABI carrier's width supplies no behavioral range.
type ResultBound =
    | Constant of int
    | Argument of int

/// The value described by a foreign behavioral contract.
type ResultRole =
    | ReturnValue

/// Provenance of an external execution promise, not a compiler-proved fact.
/// All fields must be supplied; identity, version and source participate in
/// evidence validity when the promise is used by a compiler obligation.
type ExternalExecutionHypothesis = {
    Identity: string
    Version: string
    Source: string
}

/// Exact declared receiving slot for retirement, never a C symbol. The reader
/// resolves Binding to the current release declaration and Parameter to its zero-based native ABI ordinal.
type ResourceRelease = {
    Binding: string
    Parameter: int
}

/// First resource protocol only: nullable return, owned on nonnull; only the
/// exact release is permitted, with no foreign retention or transfer. Release
/// has a raw Void ABI and reports no cleanup status. Source must explicitly
/// release on every admitted exit after a nonnull acquisition, including later
/// failure exits, retiring all aliases. Null/None holds no resource and must not
/// release. This descriptor inserts no cleanup. Other protocols refuse.
type ResourceProtocol =
    | OwnedNullableReturn of ResourceRelease

/// A sibling <declaration>ResourceDescriptor quotation, associated with the
/// exact declaration body by its existing module/binding owner, never CName.
/// This promise supplies no spatial access, allocator, loading or proof authority.
type ResourceDescriptor = {
    Resource: ResourceProtocol
    Hypothesis: ExternalExecutionHypothesis
}

/// Native entry ABI without an invented C symbol or ownership default.
type EntrySignature = {
    Parameters: ParameterInfo array
    ReturnType: TypeRef
    CallingConvention: CallConv
}

/// Exact receiving extern/ordinal and its declared activation/retirement quotation. Scope names
/// the sibling scope quotation: a ScopedCallbackDescriptor (during the call), a
/// RetainedCallbackDescriptor (until a declared release) or a ProcessExitCallbackDescriptor.
type CallbackInvocationContract = {
    Binding: string
    Parameter: int
    Signature: EntrySignature
    Hypothesis: ExternalExecutionHypothesis
    Scope: string
}

/// An independent promise that the exact receiving extern/ordinal accepts
/// absence of an entry. This declares neither activation nor invocation count.
/// Missing correspondence leaves nullable acceptance Pending.
type CallbackAbsenceContract = {
    Binding: string
    Parameter: int
    Hypothesis: ExternalExecutionHypothesis
}

/// An entry argument that receives, unchanged, the opaque value the caller passed to the
/// receiving extern at ContextParameter (Farscape #189): the `void*` first argument of SQLite's
/// sqlite3_exec row callback is sqlite3_exec's own fourth argument. Binding and Parameter name
/// the receiving extern and its function-pointer ordinal, as in CallbackInvocationContract;
/// Argument is the entry's zero-based argument ordinal and ContextParameter the receiving
/// extern's zero-based native ordinal. Both are untyped data pointers the binding never
/// dereferences. The caller owns whatever they address: this grants no lifetime, type or
/// ownership authority over it, and no environment is allocated or retained for it.
type CallbackContextDescriptor = {
    Binding: string
    Parameter: int
    Argument: int
    ContextParameter: int
}

/// The element of a counted entry array (Farscape #190).
type CountedElement =
    /// A NUL-terminated byte string that is never NULL.
    | Text
    /// A NUL-terminated byte string, or NULL (SQLite's SQL NULL column value).
    | NullableText
    /// An opaque handle to the named marker that is never NULL (SQLite's protected sqlite3_value).
    | Handle of marker: string
    /// An opaque handle to the named marker, or NULL.
    | NullableHandle of marker: string

/// A pointer-to-pointer entry argument read as a bounded read-only array (Farscape #190): its
/// length is the entry argument at CountArgument of the same invocation, and each element is
/// Element (sqlite3_exec's row values and column names counted by the column count; the
/// sqlite3_value argv of an SQL function counted by argc). Binding and Parameter name the
/// receiving extern and its function-pointer ordinal; Argument and CountArgument are zero-based
/// entry argument ordinals. The foreign caller owns the array and every element; both are
/// borrowed only until the entry returns, never written and never retained. The Pointer
/// representation in the entry signature is unchanged: this states how it is read. A negative
/// or zero count admits no element; nothing here bounds the count from above.
type CountedEntryArrayDescriptor = {
    Binding: string
    Parameter: int
    Argument: int
    CountArgument: int
    Element: CountedElement
}

/// What retires a retained callback registration (Farscape #191).
type RetentionRelease =
    /// A later call of the receiving binding whose arguments at these zero-based native ordinals
    /// equal the registering call's replaces the registration (SQLite's redefinition of one
    /// function name, arity and text encoding on one connection).
    | Redefinition of key: int array
    /// A call of the exact release declaration whose Parameter receives the registration's owner
    /// (sqlite3_close_v2 of the connection).
    | OwnerRelease of ResourceRelease

/// A callback the library keeps past the registering call and may invoke at any later time
/// until a declared release retires its registration (Farscape #191). Binding is the receiving
/// extern and Parameter its function-pointer parameter, as in ScopedCallbackDescriptor; Owner is
/// the receiving extern's zero-based native ordinal whose argument holds the registration (the
/// connection). The entry is capture-free: application code supplies a closed module function,
/// and no environment is allocated or retained for it, so a retained entry has nothing of the
/// caller's to outlive. Retirement is the library's promise, stated in the invocation contract's
/// hypothesis; nothing here proves it, and no release is inserted.
type RetainedCallbackDescriptor = {
    Binding: string
    Parameter: string
    Owner: int
    Releases: RetentionRelease array
}

/// What a borrowed result's bytes are (Farscape #196).
type BorrowedContent =
    /// UTF-8 text; the extent excludes any terminating NUL.
    | Utf8Text
    /// Uninterpreted bytes.
    | RawBytes

/// How a borrowed result's byte extent is found (Farscape #196).
type BorrowedExtent =
    /// The integer result of a call of the exact declaration Binding whose arguments are the
    /// borrowing call's arguments at these zero-based native ordinals, in order, made after the
    /// borrowing call and before any invalidation (sqlite3_column_bytes(stmt, iCol) after
    /// sqlite3_column_text(stmt, iCol): SQLite measures the text it just converted).
    | CompanionCall of binding: string * arguments: int array
    /// The bytes before the first NUL byte (sqlite3_errmsg's message).
    | NulTerminated

/// What ends a borrowed result's validity (Farscape #196).
type BorrowInvalidation =
    /// A call of the exact declaration Binding whose Parameter receives the owner
    /// (sqlite3_step, sqlite3_reset or sqlite3_finalize of the statement).
    | OwnerCall of ResourceRelease
    /// Any later call of a binding that receives the owner (sqlite3_errmsg's message, which the
    /// connection's next call may overwrite).
    | AnyOwnerCall

/// A pointer result read as a borrowed read-only view of Content (Farscape #196): SQLite's
/// sqlite3_column_text and sqlite3_column_blob, sqlite3_value_text and sqlite3_errmsg. Binding is
/// the exact qualified generated extern declaration; Owner is its zero-based native ordinal whose
/// argument owns the memory (the statement, the value, the connection). The library owns the bytes:
/// the view never writes, frees or retains them, and is valid only until the first InvalidatedBy
/// event on the same owner (and never past the owner itself). A NULL result is no view. The
/// FunctionDescriptor's Pointer result representation is unchanged: this states how it is read.
/// Extent and validity are the library's promise, stated in Hypothesis; nothing here proves them.
type BorrowedResultViewDescriptor = {
    Binding: string
    Content: BorrowedContent
    Extent: BorrowedExtent
    Owner: int
    InvalidatedBy: BorrowInvalidation array
    Hypothesis: ExternalExecutionHypothesis
}

/// A sibling quoted declaration of result behavior. Binding is the fully
/// qualified Clef extern declaration, never merely its C symbol. This preserves
/// FunctionDescriptor's ABI vocabulary and grants no loading or ownership
/// authority. Absence leaves dependent obligations Pending; it invents no range.
type ResultRangeDescriptor = {
    Binding: string
    Role: ResultRole
    Hypothesis: ExternalExecutionHypothesis
    Lower: ResultBound
    Upper: ResultBound
}

/// Initialized caller-owned byte storage borrowed through this call only.
/// Parameter must have an explicit unsigned-8 reference-element contract;
/// CountParameter is a distinct native ordinal measuring bytes. Foreign access
/// stays within that actual count and the borrow is not retained after return.
/// Source establishes nonnegative count, capacity and current memory/use facts.
/// Binding is the exact qualified generated extern declaration, not its C name.
/// No result, encoding, ABI, alignment, ownership or termination promise follows.
type WritableBufferDescriptor = {
    Binding: string
    Parameter: int
    CountParameter: int
    Hypothesis: ExternalExecutionHypothesis
}

/// How a borrowed read-only foreign buffer's byte extent is bounded at the C boundary.
type ReadOnlyBufferExtent =
    /// Split (ptr, count) zero-copy crossing measured by the companion native count parameter
    /// (no trailing NUL sentinel; embedded 0x00 bytes preserved).
    | ExplicitCount of countParameter: int
    /// Legacy uncounted NUL-terminated C path parameter (static rodata borrow or call-scoped [len + 1] staging).
    | UncountedNulTerminated

/// How long a foreign callee may retain read access to the caller's read-only buffer bytes.
type BufferRetention =
    /// Foreign access ends when the foreign call returns.
    | DuringCall
    /// Foreign access is retained past return (for example SQLITE_STATIC) until a matching
    /// slot redefinition or an admitted owner release retires it.
    | RetainedUntil of owner: int * releases: RetentionRelease array

/// Initialized caller-owned read-only byte or UTF-8 text storage borrowed across a foreign call.
/// Parameter is the zero-based pointer ordinal; Extent selects split explicit-count or legacy
/// NUL-terminated path marshaling; Content distinguishes UTF-8 text from raw bytes; Retention
/// states whether access ends on return or persists until owner redefinition/release.
/// Binding is the exact qualified generated extern declaration, not its C name.
type ReadOnlyBufferDescriptor = {
    Binding: string
    Parameter: int
    Extent: ReadOnlyBufferExtent
    Content: BorrowedContent
    Retention: BufferRetention
    Hypothesis: ExternalExecutionHypothesis
}

/// Explicit contract that an optional foreign buffer or string parameter admits None -> 0 (NULL)
/// strictly at the foreign call site. Binding is the fully qualified Clef extern declaration, and
/// Parameter is its zero-based native ordinal.
type BufferAbsenceContract = {
    Binding: string
    Parameter: int
    Hypothesis: ExternalExecutionHypothesis
}

/// A generated listener field's native calling contract. The record and field
/// identify each FnPtr supplied by application code; the signature fixes its
/// C parameter and result representations independently of the Clef carrier.
type CallbackDescriptor = {
    Record: string
    Field: string
    Signature: FunctionDescriptor
}

/// A binding-owned adapter for a closed application handler targeting a one-field
/// native listener record (`FnPtr`), actively consumed by
/// `Clef.Compiler.Nanopass.ClosedCallbacks`. Binding names a one-argument factory
/// returning the native listener record. Adapter names a module function taking
/// that handler followed by the native entry arguments. The compiler specializes
/// it to a named module handler without captures; no closure environment is
/// retained. CallbackDescriptor still declares the record field's complete C
/// ABI (`Signature: FunctionDescriptor`), whereas direct `TypeRef.Entry` callback
/// parameters use `EntrySignature` inside `CallbackInvocationContract` together
/// with `CallbackAbsenceContract`. These names are fully qualified bindings.
type ClosedCallbackDescriptor = {
    Binding: string
    Adapter: string
    Record: string
    Field: string
}

/// A borrowed view's element representation. Schema names the opaque phantom
/// marker, not a C struct; its element width never changes ordinary Clef arrays.
type ViewLayoutDescriptor = {
    Schema: string
    Element: BAREWire.Hardware.Repr
    Alignment: int
    Access: BAREWire.Hardware.AccessKind
}

/// Declared foreign call roles for one SharedBufferPlacement (Farscape #205):
/// Placement names the SharedBufferPlacement.Name; Acquire names the exact qualified
/// [<FidelityExtern>] declaration (or C symbol) that maps/acquires the shared region;
/// Release names the exact qualified [<FidelityExtern>] declaration (or C symbol) that
/// unmaps/releases the region (Some release when LeakedAtExit = false, None when
/// LeakedAtExit = true); Sync names the exact qualified [<FidelityExtern>] declarations
/// (or C symbols) that flush/synchronize the mapped region; ForeignUses names any
/// additional [<FidelityExtern>] declarations borrowing the mapped region while live.
type SharedBufferRegionDescriptor = {
    Placement: string
    Acquire: string
    Release: string option
    Sync: string array
    ForeignUses: string array
}

/// Values carried by one native acquisition. Output names a single declared
/// reference cell written by that call, including a nullable opaque cookie.
type MappedValue =
    | Input of string
    | Output of string

/// A generated scoped wrapper over an existing native map/unmap pair. Native
/// FunctionDescriptors retain their exact arities. The compiler owns output
/// cells, validates stride/extent/alignment, invokes the callback with a borrowed
/// view, and releases once after every callback use has retired. Failed native
/// acquisition never invokes the callback or the release binding.
type MappedReturnDescriptor = {
    Binding: string
    Acquire: string
    Release: string
    Layout: string
    CallbackParameter: string
    Owner: MappedValue
    RowStride: MappedValue
    RowCount: MappedValue
    RowWidth: MappedValue
    ReleaseArguments: MappedValue array
    FailureStatus: int
}

/// The binding promises that every use and capture of this callback retires
/// before it returns. This declaration is a trusted library obligation. The
/// compiler checks callers' scoped uses against that promise; it does not prove
/// an arbitrary scheduling implementation honors it. Native lifecycle tests
/// and implementation review supply separate retirement evidence.
type ScopedCallbackDescriptor = {
    Binding: string
    Parameter: string
}

/// Conditional registration for any normal process termination. The optional
/// exit declaration refines a test/site; absence does not mean before-return.
/// Invocation provenance is supplied by CallbackInvocationContract.Hypothesis.
/// SuccessStatus supplies no result range, code residence or proof of success.
type ProcessExitCallbackDescriptor = {
    Binding: string
    Parameter: string
    ExitBinding: string option
    SuccessStatus: int
}

/// Parameter constructors.
module Parameter =

    /// A parameter passed by value.
    let value (name: string) (t: TypeRef) : ParameterInfo =
        { Name = name; Type = t; PassBy = Value }

    /// A parameter passed by reference.
    let reference (name: string) (t: TypeRef) : ParameterInfo =
        { Name = name; Type = t; PassBy = Reference }

    /// A reference whose foreign contract excludes writes through this parameter.
    let readOnlyReference (name: string) (t: TypeRef) : ParameterInfo =
        { Name = name; Type = t; PassBy = ReadOnlyReference }

/// Function descriptor constructors. A descriptor is a declaration the compiler
/// reads (CCS8206 for a shape it cannot follow, CCS8207 for a width of no bits);
/// nothing here evaluates one.
module Function =

    /// A C-convention function whose ownership transfer is undeclared.
    let cdecl (cname: string) (parameters: ParameterInfo array) (returnType: TypeRef) : FunctionDescriptor =
        { CName = cname; Parameters = parameters; ReturnType = returnType; CallingConvention = CDecl; OwnershipTransfer = Undeclared }

    /// The same descriptor with the ownership transfer declared.
    let withTransfer (f: FunctionDescriptor) (transfer: Transfer) : FunctionDescriptor =
        { CName = f.CName; Parameters = f.Parameters; ReturnType = f.ReturnType; CallingConvention = f.CallingConvention; OwnershipTransfer = transfer }

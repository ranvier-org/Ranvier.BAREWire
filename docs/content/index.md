---
title: Overview
description: BARE binary codecs, envelope framing, duplex transport, and Ranvier.Query bindings for .NET, NativeAOT, and Fable.
---

# Ranvier.BAREWire

[BAREWire](https://forge.spkez.dev/FidelityFramework/BAREWire) packaged as a Ranvier primitive: its zero-copy binary encoder, decoder, schema emitter, and envelope framing across every Ranvier target, paired with typed codecs and [Ranvier.Query](repo:core/guide/queries.md) bindings.

| Package | Purpose |
| --- | --- |
| `Ranvier.BAREWire` | Vendored `BAREWire` modules plus `BareCodec<'T>`, `BareFrame`, `BareClient`, and `BareDuplex` |
| `Ranvier.BAREWire.Query` | `QueryClient.DefineBare`, `QueryClient.MutateBare`, and graph-thread `BareDuplex.OnEvent` delivery |
| `Ranvier.BAREWire.Traced` | Traced build of `Ranvier.BAREWire` (`RanvierTrace=true`) |
| `Ranvier.BAREWire.Query.Traced` | Traced build of `Ranvier.BAREWire.Query` (`RanvierTrace=true`) |

## Installation

```bash
dotnet add package Ranvier.BAREWire
dotnet add package Ranvier.BAREWire.Query
```

When building against a traced Ranvier graph (`Ranvier.Traced` and `Ranvier.Query.Traced`), reference the matching `.Traced` package variants instead:

```bash
dotnet add package Ranvier.BAREWire.Traced
dotnet add package Ranvier.BAREWire.Query.Traced
```

## Targets and runtime compatibility

Both packages target **.NET 10**, **.NET Standard 2.1**, and **Fable** (shipping F# sources for `fickl` / Fable compilation). There is no JSON dependency anywhere in the stack.

- **Upstream BAREWire compatibility:** BAREWire's modules keep their original namespaces (`BAREWire.Encoding`, `BAREWire.Framing`, `BAREWire.Schema`, `BAREWire.Memory`, `BAREWire.Hardware`, `BAREWire.Platform`). Hand-written or schema-generated BAREWire readers and writers compile directly against `Ranvier.BAREWire`. Do not reference a separate build of `BAREWire` alongside `Ranvier.BAREWire`, as both assemblies would define the same types.
- **NativeAOT and trimming:** `.NET 10` builds are trimmable and NativeAOT-compatible. Under NativeAOT, nesting generic codec value types more than roughly five levels deep (for example, lists of maps of options of lists) can hit the compiler's generic instantiation depth limit; flatten deep hierarchies by naming intermediate records with `bareStruct`, which introduces no nested generic type arguments.
- **UTF-8 handling:** BAREWire encodes unpaired UTF-16 surrogates as `U+FFFD` and decodes ill-formed UTF-8 sequences as `U+FFFD` without faulting.

## Guides

- [Codecs and framing](codecs-and-framing.md) — `BareCodec<'T>`, `bareStruct`, schema validation, `BareFrame`, `BareClient`, and `BareDuplex`.
- [Ranvier.Query bindings](queries.md) — `QueryClient.DefineBare`, `QueryClient.MutateBare`, and graph-dispatched server events.

## Vendored BAREWire maintenance

BAREWire's upstream sources live under `src/Ranvier.BAREWire/Vendor/BAREWire` and compile directly into `Ranvier.BAREWire`. Never hand-edit files inside `Vendor/BAREWire`; update them from upstream with `scripts/vendor-barewire.ps1` or the `build.fsx` targets:

```bash
# Vendor the tip of upstream main
pwsh scripts/vendor-barewire.ps1
dotnet fsi --exec build.fsx -- vendor

# Vendor a specific branch, tag, or commit
pwsh scripts/vendor-barewire.ps1 -Ref <branch-tag-or-commit>
dotnet fsi --exec build.fsx -- vendor --ref <branch-tag-or-commit>

# Verify committed vendor files match the commit recorded in VENDORED.md
pwsh scripts/vendor-barewire.ps1 -Check
dotnet fsi --exec build.fsx -- vendor-check
```

`vendor-barewire.ps1` copies every source compiled by upstream `src/BAREWire.fsproj` byte for byte, generates `BAREWire.props` in upstream compile order, records provenance in `VENDORED.md`, copies `LICENSE`, `PATENTS.md`, and `Commercial.md`, and merges upstream `Float.Fable.fs` and `Float.Dotnet.fs` into a single `#if FABLE_COMPILER` / `#else` `Encoding/Substrate/Float.fs` shim so NuGet ships one unified source list for both .NET and Fable.

After vendoring an upstream commit, run the full verification suite:

```bash
dotnet fsi --exec build.fsx -- ci
```

This checks formatting, runs the untraced and traced Expecto suites (`--group dotnet`), compiles and runs the Fable smoke checks under Node (`--group fable`), publishes and runs the rooted NativeAOT binary (`--group aot`), and verifies package licence metadata (`MIT AND Apache-2.0` with `NOTICE.md` and `licenses/BAREWire/`).

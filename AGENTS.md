# AGENTS.md

## Repository

Ranvier.BAREWire holds [BAREWire](https://forge.spkez.dev/FidelityFramework/BAREWire) for Ranvier. The core packages
(`Ranvier`, `Ranvier.Query`) are pinned NuGet dependencies, set by the `Ranvier*Version` properties in
`Directory.Build.props`. `dotnet fsi build.fsx -- dev local` switches them to project references into a sibling
`../Ranvier` checkout; `dev packages` switches back.

- `src/Ranvier.BAREWire`: the library, packed with its Fable sources. It compiles the vendored BAREWire sources
  first, then `Types.fs`, `BareCodec.fs`, `Frames.fs` and `Session.fs`.
- `src/Ranvier.BAREWire/Vendor/BAREWire`: BAREWire's sources and licence files.
- `src/Ranvier.BAREWire.Query`: the Ranvier.Query bindings.
- `tests/Ranvier.BAREWire.Tests`: the Expecto suite.
- `tests/Ranvier.BAREWire.Smoke/Checks.fs`: checks compiled by `tests/Ranvier.BAREWire.AotSmoke` (NativeAOT) and
  `fable/Ranvier.BAREWire.Smoke.Fable` (fickl, run under Node).

Every package also packs as `<PackageId>.Traced` with `-p:RanvierTrace=true`, against the traced core packages.
Debug builds are traced by default. Run every check through `build.fsx`; `test --group` names a CI shard.

## The vendored folder

Never edit a file under `src/Ranvier.BAREWire/Vendor/BAREWire`. `scripts/vendor-barewire.ps1` writes all of it from
BAREWire's repository, and `-Check` fails on any difference. A change BAREWire needs is made upstream, then
vendored with `pwsh scripts/vendor-barewire.ps1 -Ref <commit>`.

BAREWire is Apache-2.0 and this repository is MIT. Keep them apart: do not copy BAREWire code into a file outside
the vendored folder, and do not remove `NOTICE.md` or the `licenses/BAREWire/` items from the package.

## Writing codecs and library code

- BAREWire's cursor convention holds everywhere: a writer returns the next offset or `Cursor.Fault`, a reader returns
  `Unchecked.defaultof` with `Cursor.Fault`, and a fault offset in is a fault offset out. Never call a user function
  on a value read at a fault.
- A codec's `Size` is exact. `Encode` raises when the writer ends anywhere else.
- Keep generic types shallow. A type that nests one level per element, as an applicative `and!` builder does, fails
  NativeAOT with `IL3054`: measured here on 2026-11-16, a six-field struct built that way did not publish.
  `bareStruct` binds fields in sequence for that reason.
- The library projects compile with `--reflectionfree`: no `sprintf "%A"`, no `printf` of a record or union.
- Fable cannot type-test an interface. Test for a class.

## Before reporting work as done

`dotnet fsi build.fsx -- ci` runs every gate. A filtered test run is not an acceptance check.

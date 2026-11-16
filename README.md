# Ranvier.BAREWire

[BAREWire](https://forge.spkez.dev/FidelityFramework/BAREWire) as a Ranvier primitive: its encoder, decoder,
schema and envelope framing on every Ranvier target, with typed codecs and Ranvier.Query bindings over them.

| Package | What it holds |
|---|---|
| [`Ranvier.BAREWire`](src/Ranvier.BAREWire/README.md) | BAREWire itself, vendored, and `BareCodec<'T>`, `BareFrame`, `BareClient` and `BareDuplex` over it |
| [`Ranvier.BAREWire.Query`](src/Ranvier.BAREWire.Query/README.md) | `QueryClient.DefineBare` and `MutateBare`, and events delivered on the graph's thread |

Each also packs as `<PackageId>.Traced` for a traced graph build, as the other Ranvier packages do. Targets: .NET 10,
.NET Standard 2.1 and Fable; the .NET 10 builds are NativeAOT-compatible.

## Vendored BAREWire

BAREWire's sources live in `src/Ranvier.BAREWire/Vendor/BAREWire` and compile into the `Ranvier.BAREWire` assembly
under their own namespaces. One script writes that folder from BAREWire's repository:

```bash
pwsh scripts/vendor-barewire.ps1
```

```bash
pwsh scripts/vendor-barewire.ps1 -Ref <branch, tag or commit>
```

```bash
pwsh scripts/vendor-barewire.ps1 -Check
```

The first form vendors the tip of `main`. `-Check` fetches the commit recorded in `VENDORED.md` and fails when a
committed file differs from it. `dotnet fsi build.fsx -- vendor [--ref <ref>]` and `-- vendor-check` run the same
script.

The script copies every file BAREWire's own `BAREWire.fsproj` compiles, byte for byte, and writes the compile list
(`BAREWire.props`) from that project file, so a newer commit that adds or reorders files needs no edit here. One
file is generated: `Float.fs` holds BAREWire's two float shims unmodified, one under `#if FABLE_COMPILER` and one
under `#else`, because a NuGet package ships a single source list for .NET and Fable. Never edit a vendored file:
change it upstream and run the script.

After vendoring a new commit, run `dotnet fsi build.fsx -- ci`. The NativeAOT and Fable smoke programs compile every
vendored file, so an upstream change that leaves either target fails there.

## Licensing

Ranvier's code here is under the MIT License (`LICENSE`). BAREWire is copyright SpeakEZ Technologies, Inc. and
licensed under the Apache License 2.0, with a commercial licence available. The `Ranvier.BAREWire` package carries
the licence expression `MIT AND Apache-2.0` and ships BAREWire's `LICENSE`, `PATENTS.md` and `Commercial.md` under
`licenses/BAREWire/`, with `NOTICE.md` stating what is included and that nothing was modified.
`.github/scripts/vendored.cjs` fails the pack when a package lacks any of them.

## Development

```bash
dotnet fsi build.fsx -- dev local
```

```bash
dotnet fsi build.fsx -- ci
```

`dev local` references the sibling `../Ranvier` checkout as projects; `dev packages` returns to the published
`Ranvier` and `Ranvier.Query` packages pinned in `Directory.Build.props`. `ci` checks formatting, builds, runs the
three test groups and packs. A single group runs with `test --group dotnet`, `fable` or `aot`.

- `tests/Ranvier.BAREWire.Tests`: the Expecto suite, run untraced and traced.
- `tests/Ranvier.BAREWire.Smoke/Checks.fs`: one set of checks compiled by the next two projects.
- `tests/Ranvier.BAREWire.AotSmoke`: a NativeAOT publish with every assembly rooted and warnings as errors.
- `fable/Ranvier.BAREWire.Smoke.Fable`: the same checks compiled with fickl and run under Node.

On Windows the NativeAOT group needs `vswhere.exe` on `PATH`
(`C:\Program Files (x86)\Microsoft Visual Studio\Installer`). It also needs a `PATH` short enough for the MSVC
environment script: when the publish prints `The input line is too long` and then fails in `link`, the build ran
Git's `link` in place of the linker. Run that group from a shell with a shorter `PATH`.

## Releasing

Releases follow the ShipIt flow the other Ranvier repositories use (`CONTRIBUTOR.md`):

1. Squash-merge a pull request to `main` whose title is a conventional commit. `feat` and `fix` cut a release.
2. When CI passes, `shipit.yml` opens or updates the `release/main` pull request, titled `chore: release …`.
3. Merge that pull request with its title unchanged. CI packs the `nuget-packages` artifact and `publish.yml`
   pushes exactly those files to nuget.org through trusted publishing.

Before the first release, three things must exist outside this repository: the GitHub repository
`ranvier-org/Ranvier.BAREWire`, a nuget.org trusted-publishing policy for that repository's `publish.yml`, and,
optionally, the `SHIPIT_TOKEN` secret so that the release pull request's CI starts by itself.

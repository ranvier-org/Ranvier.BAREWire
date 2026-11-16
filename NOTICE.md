# Notice

Ranvier.BAREWire is copyright (c) 2026 Shayan Habibi and licensed under the MIT License (`LICENSE`).

## BAREWire

The `Ranvier.BAREWire` package and assembly include BAREWire, copyright 2025 SpeakEZ Technologies, Inc.,
licensed under the Apache License, Version 2.0, with a commercial licence available from SpeakEZ Technologies.

- Source: https://forge.spkez.dev/FidelityFramework/BAREWire
- Location in this repository: `src/Ranvier.BAREWire/Vendor/BAREWire`
- Commit vendored, and the list of files: `src/Ranvier.BAREWire/Vendor/BAREWire/VENDORED.md`
- Licence texts: `LICENSE`, `PATENTS.md` and `Commercial.md` in that folder, shipped in the NuGet package under
  `licenses/BAREWire/`

Changes to BAREWire's files, as section 4(b) of the Apache License asks to be stated: none. Each vendored source
file is BAREWire's file byte for byte. One file, `src/Encoding/Substrate/Float.fs`, is generated: it holds
BAREWire's `Float.Fable.fs` and `Float.Dotnet.fs`, each unmodified, under `#if FABLE_COMPILER` and `#else`, so that
one source list serves .NET and Fable. `scripts/vendor-barewire.ps1` writes every vendored file, and
`pwsh scripts/vendor-barewire.ps1 -Check` verifies them against the recorded commit.

BAREWire's patent notice (`PATENTS.md`) names U.S. Provisional Patent Application No. 63/786,247 and grants the
patent licence of section 3 of the Apache License to recipients under that licence. That grant comes from SpeakEZ
Technologies, Inc., not from the authors of Ranvier.BAREWire.

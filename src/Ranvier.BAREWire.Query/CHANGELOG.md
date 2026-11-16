---
last_commit_released: 4af973885ab208076fa57394ba0da0ad6ab6f744
include:
  - ../../Directory.Build.props
depends_on:
  - ../Ranvier.BAREWire/
updaters:
  - xml:
      file: Ranvier.BAREWire.Query.fsproj
      selector: /Project/PropertyGroup/Version
---

# Changelog

This changelog is generated from conventional commits by [EasyBuild.ShipIt](https://github.com/easybuild-org/EasyBuild.ShipIt).

## 0.1.0 - 2026-11-16

### 🚀 Features

* Initialise Ranvier.BAREWire ([4af9738](https://github.com/ranvier-org/Ranvier.BAREWire/commit/4af973885ab208076fa57394ba0da0ad6ab6f744))

### 🔗 Dependencies

* `src/Ranvier.BAREWire` updated to 0.1.0

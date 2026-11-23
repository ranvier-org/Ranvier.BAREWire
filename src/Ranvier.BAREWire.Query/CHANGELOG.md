---
last_commit_released: 83409dd875c4107d4b8ca949c8e5bc486a6ebb09
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

## 0.1.1 - 2026-11-23

### 🐞 Bug Fixes

* *(deps)* Align Ranvier 0.2.1 and fickl 0.12.0 ([83409dd](https://github.com/ranvier-org/Ranvier.BAREWire/commit/83409dd875c4107d4b8ca949c8e5bc486a6ebb09))

### 🔗 Dependencies

* `src/Ranvier.BAREWire` updated to 0.1.1

<strong><small>[View changes on Github](https://github.com/ranvier-org/Ranvier.BAREWire/compare/4af973885ab208076fa57394ba0da0ad6ab6f744..83409dd875c4107d4b8ca949c8e5bc486a6ebb09)</small></strong>

## 0.1.0 - 2026-11-16

### 🚀 Features

* Initialise Ranvier.BAREWire ([4af9738](https://github.com/ranvier-org/Ranvier.BAREWire/commit/4af973885ab208076fa57394ba0da0ad6ab6f744))

### 🔗 Dependencies

* `src/Ranvier.BAREWire` updated to 0.1.0

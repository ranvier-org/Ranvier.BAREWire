---
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

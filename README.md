# Wave Function Collapse

A set of solvers, prototypes and tools to explore Wave Function Collapse. The
solver in this repository is used by
[Monoceros](https://github.com/subdgtl/Monoceros)

Currently includes:

- `wfc_core`: A Rust library implementing a WFC solver in 3d voxel space,

- `wfc_cli`: A Rust command-line app exposing the solver in `wfc_core`,

- `wfc_dylib`: A dynamic library (`.dll`, `.so`, `.dylib`) exposing the solver
  in `wfc_core` to use from other sorfware capable of talking to C,

- `wfc_gh`: A simple C# Grasshopper component utilizing `wfc_dylib` as its
  solver (also see [Monoceros](https://github.com/subdgtl/Monoceros) for a
  complete Grasshopper implementation).

## Developing

`wfc_core`, `wfc_cli` and `wfc_dylib` are built with Rust's Cargo tool and
follow standard Cargo workflows. Get rust via [Rustup](https://rustup.rs/).

`wfc_gh` is built with Visual Studio, see `README.md` in `wfc_gh` for more.

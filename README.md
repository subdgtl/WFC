# wfc1

A set of prototype tools for playing with Wave Function Collapse. Currently includes:

- `wfc_core`: A Rust library implementing solvers for WFC, currently only a
  solver in 3d voxel-space

- `wfc_cli`: A Rust command-line app exposing `wfc_core` for 3d voxel-space
  based on custom CSV and TXT formats

- `wfc_dylib`: A dynamic library (`.dll`, `.so`, `.dylib`) exposing
  functionality of `wfc_core` to use from other sorfware

- `wfc_gh`: A C# Grasshopper component utilizing `wfc_dylib`

## Developing

`wfc_core`, `wfc_cli` and `wfc_dylib` are built with Rust's `cargo
tool`. `wfc_gh` has to be built with Visual Studio, see `README.md` in `wfc_gh`
for more.

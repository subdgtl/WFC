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
The required toolchain version is pinned in the `rust-toolchain` file at
the workspace root and will be installed automatically by Rustup the first
time you run a Cargo command.

The Rust crates build on:

- Linux (`x86_64-unknown-linux-gnu`, `aarch64-unknown-linux-gnu`)
- Windows (`x86_64-pc-windows-msvc`)
- Intel Mac (`x86_64-apple-darwin`)
- Apple Silicon Mac (`aarch64-apple-darwin`, e.g. M1/M2/M3/M4)

To build everything in release mode:

```
cargo build --release
```

On an Apple Silicon Mac this produces `target/release/libwfc.dylib`
(the `wfc_dylib` C ABI library) and `target/release/wfc_cli` as native
arm64 binaries. If you need a universal (fat) binary that runs on both
Intel and Apple Silicon Macs, build each target separately and stitch
them together with `lipo`:

```
rustup target add x86_64-apple-darwin aarch64-apple-darwin
cargo build --release --target x86_64-apple-darwin
cargo build --release --target aarch64-apple-darwin
lipo -create -output target/libwfc.dylib \
    target/x86_64-apple-darwin/release/libwfc.dylib \
    target/aarch64-apple-darwin/release/libwfc.dylib
```

`wfc_gh` is a C# Grasshopper component for Rhino on Windows and is built
with Visual Studio against the .NET Framework; see `README.md` in
`wfc_gh` for more. It is not part of the Apple Silicon build.

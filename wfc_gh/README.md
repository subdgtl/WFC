# wfc_gh

A Grasshopper plugin utilizing the solver in `wfc_core` via `wfc_dylib`.

## Developing

**First time setup**

This requires Visual Studio 2019 and .NET Framework 4.8 SDK on the developer
machine.

Get the Framework SDK here:
https://dotnet.microsoft.com/download/visual-studio-sdks?utm_source=getdotnetsdk&utm_medium=referral

**Building**

- Build with Visual Studio in release mode

- Rename the output file `wfc_gh.dll` (will likely be located in
  `project_dir/wfc_gh/bin/Release/net48`) to `wfc_gh.gha`

- Copy this file to a directory where Grasshopper looks for libraries
  (e.g. `AppData/Roaming/Grasshopper/Libraries`) along with `wfc.dll` dynamic
  library obtained by building the `wfc_dylib` project.

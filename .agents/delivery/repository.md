# WFC delivery configuration

Read `controller.md` and `policy.json`. The policy lists the complete current automated profile; it does not establish native or production acceptance.

## Setup

- Install Python 3.10 or newer on both provider environments and the separate verification machine.
- rustup using rust-toolchain
- dotnet with net48 reference assemblies

Dependency installation must not start a deployment or alter original worktrees. A missing compiler, SDK, fixture or library fails explicitly. Required setup that blocks integration counts within the same 300-second allowance.

## Integration checks

The fixed checks validate the new delivery code and its failure controls. A reviewed plan bound to the exact staged tree must additionally include compilation or type checks, lint and behavior tests for changed application code. Never use these workflow checks alone for a product change. Reuse the same integration identifier across retries and corrections. Do not call a long full gate before merging.

## Full verification

- linux: cargo test --workspace; cargo fmt --all -- --check
- windows: dotnet build wfc_gh/wfc_gh.csproj

## Release prerequisites

- Old pinned Rust and Rhino binary references need verified cloud support; no established full gate.

The release guard currently records these prerequisites as blocked. Clear a blocker only through a reviewed configuration change backed by actual source, artifact and platform evidence. Never tag or deploy while it remains blocked. This pull request starts no controller, schedule, release or deployment.

## Cloud dispatch

Either provider may control delivery. Configure the real Codex Cloud environment identifier outside Git; Claude cloud may use the environment exposed by its connector. Use `cloud.py` to prepare the actual peer request. Keep sign-in credentials outside files and prompts. The strong independent review pair is `gpt-6-astra` at `ultra` and `claude-opus-5-5` at `max`; preserve explicit owner preferences for implementers.

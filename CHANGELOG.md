# Changelog

## Unreleased

New features:

- The solver now supports up to 1014 modules. Additionally, solver performance
  now improves compared to previous versions when less modules are used. For
  more info, see [#24](https://github.com/subdgtl/WFC/pull/24)

- The API of setting slot modules and slot module weights in `wfc_dylib` is now
  more granular and straightforward. Instead of providing arrays of data
  containing slot modules or slot module weights, there's now functions that
  get/set a single module state or weight at a world position.

## v0.3.1

New features:

- The solver now optionally supports per-slot, per-module weights. These can be
  used for both weighting the slot to observe, and weighting the random choice
  of the module. This supersedes the previous Shannon Entropy option, which is
  now removed.

## v0.3.0

(not released publicly, see changelog for v0.3.1)

## v0.2.0

New features:

- The solver can now also use Shannon Entropy in addition to the already
  existing Linear Entropy strategy. Weights are computed from counting the
  multiplicity of modules in provided rules.

- The number of observations made by the solver can now be limited. The solver
  also reports the actual number of observations made.

## v0.1.0

This is the first official release.

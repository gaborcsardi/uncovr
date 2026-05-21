
# 0.0.0.9000

* Skip the `gcov` step for packages that have a `src/` directory but no
  instrumented C code (no `.gcno` files), so packages like `pak` that
  ship a `src/` without producing gcov output no longer fail with
  "gcov: Not enough positional command line arguments specified!".

First public release.

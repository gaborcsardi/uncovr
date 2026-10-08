
# 0.0.0.9000

* The build directory is now created by cloning every file of the
  package, instead of symlinking (on Unix) or copying (on Windows).
  Cloning uses `clonefile()` on macOS, the `FICLONE` ioctl on Linux and
  `FSCTL_DUPLICATE_EXTENTS_TO_FILE` on Windows. If the file system does
  not support cloning (e.g. ext4 or NTFS), files are hard linked, and if
  that fails too, copied. Build directories from older versions are
  updated automatically, their symlinks are replaced.

* Skip the `gcov` step for packages that have a `src/` directory but no
  instrumented C code (no `.gcno` files), so packages like `pak` that
  ship a `src/` without producing gcov output no longer fail with
  "gcov: Not enough positional command line arguments specified!".

First public release.

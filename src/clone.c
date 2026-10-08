// nocov start

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

/* Try to create `to` as a copy-on-write clone of the file `from`.
 *
 * Returns TRUE on success and FALSE if cloning is not possible, e.g.
 * because the file system does not support it, or `from` and `to` are on
 * different file systems. It does not throw errors, the caller is
 * expected to fall back to a regular copy. `to` must not exist. If
 * cloning fails, then `to` is not created.
 *
 * - macOS: clonefile() (APFS).
 * - Linux: the FICLONE ioctl (Btrfs, XFS, bcachefs, etc.).
 * - Windows: FSCTL_DUPLICATE_EXTENTS_TO_FILE (ReFS, Dev Drive).
 */

#ifdef _WIN32

#include <windows.h>
#include <winioctl.h>

int utf8_to_utf16(const char* s, WCHAR** ws_ptr);

#ifndef FILE_SUPPORTS_BLOCK_REFCOUNTING
#define FILE_SUPPORTS_BLOCK_REFCOUNTING 0x08000000
#endif

#ifndef FSCTL_DUPLICATE_EXTENTS_TO_FILE
#define FSCTL_DUPLICATE_EXTENTS_TO_FILE \
  CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 209, METHOD_BUFFERED, FILE_WRITE_DATA)
#endif

/* Same layout as DUPLICATE_EXTENTS_DATA, which is missing from older
 * MinGW headers. */
typedef struct {
  HANDLE FileHandle;
  LARGE_INTEGER SourceFileOffset;
  LARGE_INTEGER TargetFileOffset;
  LARGE_INTEGER ByteCount;
} cov_duplicate_extents_data;

static int clone_file(const char *from, const char *to) {
  WCHAR *wfrom, *wto;
  if (utf8_to_utf16(from, &wfrom) || utf8_to_utf16(to, &wto)) {
    return 0;
  }

  HANDLE src = CreateFileW(
    wfrom, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_DELETE, NULL,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (src == INVALID_HANDLE_VALUE) return 0;

  DWORD fsflags = 0;
  if (!GetVolumeInformationByHandleW(src, NULL, 0, NULL, NULL, &fsflags,
                                     NULL, 0) ||
      !(fsflags & FILE_SUPPORTS_BLOCK_REFCOUNTING)) {
    CloseHandle(src);
    return 0;
  }

  LARGE_INTEGER size;
  DWORD spc, bps, nfree, ntotal;
  WCHAR root[MAX_PATH];
  if (!GetFileSizeEx(src, &size) ||
      !GetVolumePathNameW(wfrom, root, MAX_PATH) ||
      !GetDiskFreeSpaceW(root, &spc, &bps, &nfree, &ntotal) ||
      spc * bps == 0) {
    CloseHandle(src);
    return 0;
  }
  LONGLONG cluster = (LONGLONG) spc * bps;

  HANDLE dst = CreateFileW(
    wto, GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_NEW,
    FILE_ATTRIBUTE_NORMAL, NULL);
  if (dst == INVALID_HANDLE_VALUE) {
    CloseHandle(src);
    return 0;
  }

  int ok = 1;
  FILE_END_OF_FILE_INFO eof;
  eof.EndOfFile = size;
  if (!SetFileInformationByHandle(dst, FileEndOfFileInfo, &eof,
                                  sizeof(eof))) {
    ok = 0;
  }

  /* Regions must be cluster aligned, so round the size up. A single call
   * must clone less than 4GB. */
  LONGLONG total = (size.QuadPart + cluster - 1) / cluster * cluster;
  LONGLONG chunk = ((1LL << 32) - 1) / cluster * cluster;
  for (LONGLONG off = 0; ok && off < total; off += chunk) {
    cov_duplicate_extents_data dup;
    DWORD ret;
    dup.FileHandle = src;
    dup.SourceFileOffset.QuadPart = off;
    dup.TargetFileOffset.QuadPart = off;
    dup.ByteCount.QuadPart = total - off < chunk ? total - off : chunk;
    if (!DeviceIoControl(dst, FSCTL_DUPLICATE_EXTENTS_TO_FILE, &dup,
                         sizeof(dup), NULL, 0, &ret, NULL)) {
      ok = 0;
    }
  }

  CloseHandle(dst);
  CloseHandle(src);
  if (!ok) DeleteFileW(wto);
  return ok;
}

#elif defined(__APPLE__)

#include <sys/clonefile.h>

static int clone_file(const char *from, const char *to) {
  return clonefile(from, to, 0) == 0;
}

#elif defined(__linux__)

#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <linux/fs.h>

static int clone_file(const char *from, const char *to) {
#ifdef FICLONE
  struct stat st;
  int src = open(from, O_RDONLY | O_CLOEXEC);
  if (src == -1) return 0;
  if (fstat(src, &st) == -1) {
    close(src);
    return 0;
  }
  int dst = open(to, O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC,
                 st.st_mode & 07777);
  if (dst == -1) {
    close(src);
    return 0;
  }
  int ok = ioctl(dst, FICLONE, src) == 0;
  close(dst);
  close(src);
  if (!ok) unlink(to);
  return ok;
#else
  return 0;
#endif
}

#else

static int clone_file(const char *from, const char *to) {
  return 0;
}

#endif

SEXP cov_clone_file(SEXP from, SEXP to) {
#ifdef _WIN32
  const char *cfrom = Rf_translateCharUTF8(STRING_ELT(from, 0));
  const char *cto = Rf_translateCharUTF8(STRING_ELT(to, 0));
#else
  const char *cfrom = Rf_translateChar(STRING_ELT(from, 0));
  const char *cto = Rf_translateChar(STRING_ELT(to, 0));
#endif
  return Rf_ScalarLogical(clone_file(cfrom, cto));
}

// nocov end

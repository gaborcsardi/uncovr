#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

#include "errors.h"
#include "winfiles.h"

SEXP c__read_file_raw(const char *cpath) {
  SEXP result = R_NilValue;
  int err;
  int fd = open_file(cpath, O_RDONLY);

  if (fd == -1) {
    return(R_FORMAT_SYSTEM_ERROR("Cannot open file `%s`", cpath));
  }

  off_t len = lseek(fd, 0, SEEK_END);
  if (len == -1) {
    err = errno;                                                       // __NO_COVERAGE__
    close(fd);                                                         // __NO_COVERAGE__
    return R_FORMAT_SYSTEM_ERROR_CODE(err, "Cannot seek `%s`", cpath); // __NO_COVERAGE__
  }
  off_t len2 = lseek(fd, 0, SEEK_SET);
  if (len2 == -1) {
    err = errno;                                                       // __NO_COVERAGE__
    close(fd);                                                         // __NO_COVERAGE__
    return R_FORMAT_SYSTEM_ERROR_CODE(err, "Cannot seek `%s`", cpath); // __NO_COVERAGE__
  }

  /* TODO: should use cleancall to close the file if allocVector fails */

  result = PROTECT(Rf_allocVector(RAWSXP, len + 1));

  ssize_t ret = read(fd, RAW(result), len);
  if (ret == -1) {
    err = errno;                                                       // __NO_COVERAGE__
    close(fd);                                                         // __NO_COVERAGE__
    UNPROTECT(1);                                                      // __NO_COVERAGE__
    return R_FORMAT_SYSTEM_ERROR_CODE(err, "Cannot read `%s`", cpath); // __NO_COVERAGE__
  }
  RAW(result)[len] = 0;

  close(fd);

  UNPROTECT(1);
  return result;
}

SEXP cov_read_file_raw(SEXP path) {
  SEXP ret = PROTECT(c__read_file_raw(CHAR(STRING_ELT(path, 0))));
  if (TYPEOF(ret) != RAWSXP) {
    R_THROW_ERROR(CHAR(STRING_ELT(ret, 0)));
  }
  UNPROTECT(1);
  return ret;
}

SEXP cov_read_lines(SEXP path) {
  SEXP ret = PROTECT(c__read_file_raw(CHAR(STRING_ELT(path, 0))));
  if (TYPEOF(ret) != RAWSXP) {
    R_THROW_ERROR(CHAR(STRING_ELT(ret, 0)));
  }

  // count number of lines first
  char *beg = (char*) RAW(ret);
  char *end = beg + XLENGTH(ret);
  char *ptr = beg;
  size_t nlines = 0;
  while (ptr < end) {
    ptr = strchr(ptr, '\n');
    if (ptr) {
      ptr++;
      nlines++;
    } else {
      nlines++;
      break;
    }
  }

  SEXP lines = PROTECT(Rf_allocVector(STRSXP, nlines));
  char *lbeg = beg;
  char *lend = beg;
  size_t lno = 0;
  while (lbeg < end) {
    lend = strchr(lbeg, '\n');
    if (lend) {
      SET_STRING_ELT(lines, lno, Rf_mkCharLen(lbeg, lend - lbeg));
      lbeg = ++lend;
      lno++;
    } else {
      SET_STRING_ELT(lines, lno, Rf_mkCharLen(lbeg, end - lbeg));
      break;
    }
  }

  UNPROTECT(2);
  return lines;
}

int parse_num(char *start, char *end, size_t *ret) {
  size_t tret = 0;
  *ret = 0;
  for (; start < end; start++) {
    if (*start == ' ') continue;
    if (*start < '0' || *start > '9') return -1;
    tret = tret * 10 + (*start - '0');
  }
  *ret = tret;
  return 0;
}

// find the number of lines in a .gcov file
// searches for the last line number, from backwards

size_t find_last_line(char *bytes, size_t len) {
  char *ptr = bytes + len, *end_line, *start_line, *start_num;
  size_t ret = 0;

  // need a loop, because last line(s) might not have a line number
  do {
    // skip trailing \n and \r
    while (ptr >= bytes && (*ptr == '\n' || *ptr == '\r' || *ptr == '\0')) {
      --ptr;
    }
    // didn't fine any data? then no lines
    if (ptr < bytes) return 0;

    // go back to the beginning of the line
    end_line = ptr;
    while (ptr >= bytes && *ptr != '\n') --ptr;

    // if not a regular line, skip entirely
    start_line = ++ptr;
    if (*ptr != ' ' && (*ptr < '0' || *ptr > '9')) {
      --ptr;
      continue;
    }

    // otherwise parse line, find first :
    while (ptr < end_line && *ptr != ':') ++ptr;
    // no colon, skip line
    if (ptr == end_line) {
      ptr = start_line - 1;
      continue;
    }

    // find second :
    start_num = ++ptr;
    while (ptr < end_line && *ptr != ':') ++ptr;
    // no colon, skip line
    if (ptr == end_line) {
      ptr = start_line - 1;
      continue;
    }

    parse_num(start_num, ptr, &ret);
    if (ret == -1) {
      ptr = start_line - 1;
      continue;
    }

    break;

  } while (1);

  return ret;
}

// for testing

SEXP c_find_last_line(SEXP bytes) {
  size_t last = find_last_line((char*) RAW(bytes), XLENGTH(bytes));
  return Rf_ScalarInteger((int) last);
}

static size_t count_functions(char *beg, char *end) {
  size_t count = 0;
  while (beg < end) {
    beg = strstr(beg, "\nfunction ");
    if (!beg) {
      return count;
    }
    beg += 9;
    count++;
  }
  return count;
}

SEXP cov_parse_gcov(SEXP path) {
  SEXP bytes = PROTECT(cov_read_file_raw(path));
  size_t nlines = find_last_line((char*) RAW(bytes), XLENGTH(bytes));
  const char *res_names[] = { "line", "coverage", "code", "" };
  SEXP res = PROTECT(Rf_mkNamed(VECSXP, res_names));
  SET_VECTOR_ELT(res, 0, Rf_allocVector(INTSXP, nlines));
  SET_VECTOR_ELT(res, 1, Rf_allocVector(INTSXP, nlines));
  SET_VECTOR_ELT(res, 2, Rf_allocVector(STRSXP, nlines));

  SEXP rlin = VECTOR_ELT(res, 0);
  SEXP rcov = VECTOR_ELT(res, 1);
  SEXP code = VECTOR_ELT(res, 2);

  // A gcov file typically contains (much) less functions than the
  // number of code lines in the file, but this is not always true, because
  // C++ templates and/or macros may generate lots of functions from the
  // same code lines. So we pre-allocate space for 10000 functions, but if
  // this is not enough, then we'll go over the file and actually count the
  // functions first.

  int numfuncprealloc = 10000;
  int funpos0[numfuncprealloc];
  int *pfunpos = funpos0;
  int numfuns = 0;
  int funline0[numfuncprealloc];
  int *pfunline = funline0;
  int funlinemiss = 0;
  SEXP funpos = PROTECT(R_NilValue);
  SEXP funline = PROTECT(R_NilValue);

  char *beg = (char*) RAW(bytes);
  char *end = beg + XLENGTH(bytes);
  char *ptr = beg;
  char *line, *last;
  size_t lastcov, lastline;

  while (ptr < end) {
    // mark functions
    if (*ptr == 'f' && !strncmp("function ", ptr, 9)) {
      if (numfuns >= numfuncprealloc) {
        size_t extrafuns = count_functions(ptr, end) + 1;
        UNPROTECT(2);
        funpos = PROTECT(Rf_allocVector(INTSXP, numfuncprealloc + extrafuns));
        funline = PROTECT(Rf_allocVector(INTSXP, numfuncprealloc + extrafuns));
        pfunpos = INTEGER(funpos);
        pfunline = INTEGER(funline);
        memcpy(pfunpos, funpos0, sizeof(int) * numfuncprealloc);
        memcpy(pfunline, funline0, sizeof(int) * numfuncprealloc);
        numfuncprealloc = numfuncprealloc + extrafuns;
      }
      pfunpos[numfuns++] = (int) (ptr - beg);
    }
    // if not a regular line, skip line
    if (*ptr != ' ' && (*ptr < '0' || *ptr > '9')) {
      while (ptr < end && *ptr != '\n') ++ptr;
      while (ptr < end && (*ptr == '\n' || *ptr == '\r')) ++ptr;
      continue;
    }

    // find first ':'
    line = ptr;
    while (ptr < end && *ptr != ':' && *ptr != '\n') ++ptr;
    // no ':', then skip line
    if (ptr < end && *ptr == '\n') {
      while (ptr < end && (*ptr == '\n' || *ptr == '\r')) ++ptr;
      continue;
    }
    if (ptr > line) {
      last = ptr - 1;
      if (*last == '-') {
        lastcov = NA_INTEGER;
      } else if (*last < '0' || *last > '9') {
        lastcov = 0;
      } else {
        parse_num(line, ptr, &lastcov);
      }
    } else {
      lastcov = 0;
    }

    // find second ':'
    line = ++ptr;
    while (ptr < end && *ptr != ':' && *ptr != '\n') ++ptr;
    // no ':', then skip line
    if (ptr < end && *ptr == '\n') {
      while (ptr < end && (*ptr == '\n' || *ptr == '\r')) ++ptr;
      continue;
    }
    parse_num(line, ptr, &lastline);
    if (lastline > 0 && lastline <= nlines) {
      INTEGER(rlin)[lastline-1] = lastline;
      INTEGER(rcov)[lastline-1] = lastcov;
      // note beginning of functions
      while (funlinemiss < numfuns) {
        pfunline[funlinemiss++] = lastline;
      }
    }

    // go to next line
    line = ++ptr;
    while (ptr < end && *ptr != '\n') ++ptr;
    if (lastline > 0 && lastline <= nlines) {
      SET_STRING_ELT(code, lastline - 1, Rf_mkCharLen(line, ptr - line));
    }
    while (ptr < end && (*ptr == '\n' || *ptr == '\r')) ++ptr;
  }

  // parse functions as well
  const char *funres_names[] = {
    "line", "name", "coverage", "returned", "blocks", ""
  };
  SEXP funres = PROTECT(Rf_mkNamed(VECSXP, funres_names));
  SET_VECTOR_ELT(funres, 0, Rf_allocVector(INTSXP, numfuns));
  SET_VECTOR_ELT(funres, 1, Rf_allocVector(STRSXP, numfuns));
  SET_VECTOR_ELT(funres, 2, Rf_allocVector(INTSXP, numfuns));
  SET_VECTOR_ELT(funres, 3, Rf_allocVector(INTSXP, numfuns));
  SET_VECTOR_ELT(funres, 4, Rf_allocVector(INTSXP, numfuns));

  SEXP name = VECTOR_ELT(funres, 1);
  int *pline = INTEGER(VECTOR_ELT(funres, 0));
  int *pcoverage = INTEGER(VECTOR_ELT(funres, 2));
  int *preturned = INTEGER(VECTOR_ELT(funres, 3));
  int *pblocks = INTEGER(VECTOR_ELT(funres, 4));

  for (size_t i = 0; i < numfuns; i++) {
    pline[i] = pcoverage[i] = preturned[i] = pblocks[i] = NA_INTEGER;
  }

  for (size_t i = 0; i < numfuns; i++) {
    pline[i] = pcoverage[i] = preturned[i] = pblocks[i] = NA_INTEGER;
    // search for end of line
    line = beg + pfunpos[i] + 9;
    char *eol = memchr(line, '\n', (size_t) (end - pfunpos[i]));
    // if no \n then give up
    if (!eol) break;

    // called
    int bak = *eol;
    *eol = 0;
    char *called = strstr(line, " called ");
    *eol = bak;
    if (!called) {
      continue;
    }
    SET_STRING_ELT(name, i, Rf_mkCharLenCE(line, called - line, CE_UTF8));
    called += 8;
    char *ce = memchr(called, ' ', eol - called);
    size_t cov;
    if (!ce || parse_num(called, ce, &cov)) {
      // if failed to parse then return line == NA for this function
      continue;
    }
    pcoverage[i] = (int) cov;
    pline[i] = pfunline[i];

    // returned
    if (memcmp(" returned ", ce, 10)) {
      continue;
    }
    called = ce + 10;
    ce = memchr(called, '%', eol - called);
    if (!ce || parse_num(called, ce, &cov)) {
      continue;
    }
    preturned[i] = cov;

    // blocks
    if (memcmp("% blocks executed ", ce, 18)) {
      continue;
    }
    called = ce + 18;
    ce = memchr(called, '%', eol - called);
    if (!ce || parse_num(called, ce, &cov)) {
      continue;
    }
    pblocks[i] = cov;
  }

  const char *res2_names[] = { "lines", "functions", "" };
  SEXP res2 = PROTECT(Rf_mkNamed(VECSXP, res2_names));
  SET_VECTOR_ELT(res2, 0, res);
  SET_VECTOR_ELT(res2, 1, funres);

  UNPROTECT(6);
  return res2;
}

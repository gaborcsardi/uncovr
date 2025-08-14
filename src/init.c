#include <dlfcn.h>

#define R_NO_REMAP
#define R_USE_C99_IN_CXX 1
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// A function to flush test coverate data to disk
#ifdef GCOV_COMPILE
void __gcov_dump();
void __gcov_reset();
SEXP cov_gcov_flush() {
  REprintf("Flushing coverage info\n");
  __gcov_dump();
  __gcov_reset();
  return R_NilValue;
}
#else
SEXP cov_gcov_flush(void) {
  return R_NilValue;
}
#endif

SEXP cov_make_counter(SEXP len);
SEXP cov_get_counts(SEXP counter);
SEXP cov_read_file_raw(SEXP path);
SEXP cov_read_lines(SEXP path);
SEXP cov_parse_gcov(SEXP path, SEXP displayname);
SEXP cov_dlsym(SEXP dll, SEXP sym) {
  DllInfo *cdll = (DllInfo*) R_ExternalPtrAddr(dll);
  const char *csym = CHAR(STRING_ELT(sym, 0));
  void *res = dlsym(cdll, csym);
  REprintf("%s pointer: %p\n", csym, res);
  return R_NilValue;
}

typedef void (*fptr_t)(void);

SEXP cov_gcov_flush_package(SEXP dll) {
  DllInfo *cdll = (DllInfo*) R_ExternalPtrAddr(dll);
  union {
    void *ptr;
    void (*dump) (void);
    void (*reset) (void);
  } fptr;

  fptr.ptr = dlsym(cdll, "__gcov_dump");
  if (!fptr.ptr) {
    Rf_error("Cannot find __gcov_dump symbol");
  }
  fptr.dump();

  fptr.ptr = dlsym(cdll, "__gcov_reset");
  if (!fptr.ptr) {
    Rf_error("Cannot find __gcov_reset symbol");
  }
  fptr.reset();

  return Rf_ScalarLogical(TRUE);
}

void cov_init_altrep(DllInfo *dll);

#define CALLDEF(name, n) \
  { #name, (DL_FUNC)&name, n }

static const R_CallMethodDef callMethods[]  = {
  CALLDEF(cov_gcov_flush, 0),
  CALLDEF(cov_make_counter, 1),
  CALLDEF(cov_get_counts, 1),
  CALLDEF(cov_read_file_raw, 1),
  CALLDEF(cov_read_lines, 1),
  CALLDEF(cov_parse_gcov, 2),
  CALLDEF(cov_dlsym, 2),
  CALLDEF(cov_gcov_flush_package, 1),
  { NULL, NULL, 0 }
};

void R_init_covxx(DllInfo *dll) {
  cov_init_altrep(dll);
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

void R_init_testthatlabs(DllInfo *dll) {
  cov_init_altrep(dll);
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

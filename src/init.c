#ifdef _WIN32
#include "windows.h"
#else
#include <dlfcn.h>
#endif

#define R_NO_REMAP
#define R_USE_C99_IN_CXX 1
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP cov_make_counter(SEXP len);
SEXP cov_get_counts(SEXP counter);
SEXP cov_read_file_raw(SEXP path);
SEXP cov_read_lines(SEXP path);
SEXP cov_parse_gcov(SEXP path);

SEXP cov_gcov_flush_package(SEXP dllhandle) {
  void *cdllhandle = (void *) R_ExternalPtrAddr(dllhandle);

  DL_FUNC ptr;
#ifdef _WIN32
  ptr = (DL_FUNC) GetProcAddress((HMODULE)cdllhandle, "__gcov_dump");
#else
  ptr = (DL_FUNC) dlsym(cdllhandle, "__gcov_dump");
#endif
  if (!ptr) {
    Rf_error("Cannot find __gcov_dump symbol");
  }
  ptr();

#ifdef _WIN32
  ptr =(DL_FUNC) GetProcAddress((HMODULE)cdllhandle, "__gcov_reset");
#else
  ptr =(DL_FUNC) dlsym(cdllhandle, "__gcov_reset");
#endif
  if (!ptr) {
    Rf_error("Cannot find __gcov_reset symbol");
  }
  ptr();

  return Rf_ScalarLogical(TRUE);
}

#ifdef COV_BUILD_SAFE
#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define LOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) | FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))
SEXP cov_lock_base() {
  LOCK_FRAME(R_BaseEnv);
  return Rf_ScalarLogical(1);
}
SEXP cov_unlock_base() {
  UNLOCK_FRAME(R_BaseEnv);
  return Rf_ScalarLogical(1);
}
#else
SEXP cov_lock_base() {
  return Rf_ScalarLogical(0);
}
SEXP cov_unlock_base() {
  return Rf_ScalarLogical(0);
}
#endif

void cov_init_altrep(DllInfo *dll);

#define CALLDEF(name, n) \
  { #name, (DL_FUNC)&name, n }

static const R_CallMethodDef callMethods[]  = {
  CALLDEF(cov_make_counter, 1),
  CALLDEF(cov_get_counts, 1),
  CALLDEF(cov_read_file_raw, 1),
  CALLDEF(cov_read_lines, 1),
  CALLDEF(cov_parse_gcov, 1),
  CALLDEF(cov_gcov_flush_package, 1),
  CALLDEF(cov_lock_base, 0),
  CALLDEF(cov_unlock_base, 0),
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

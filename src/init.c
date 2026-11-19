#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP cov_make_counter(SEXP len);
SEXP cov_get_counts(SEXP counter);

void cov_init_altrep(DllInfo *dll);

#define CALLDEF(name, n) \
  { #name, (DL_FUNC)&name, n }

static const R_CallMethodDef callMethods[]  = {
  CALLDEF(cov_make_counter, 1),
  CALLDEF(cov_get_counts, 1),
  { NULL, NULL, 0 }
};

// For a copy of the DLL, used when measuring the coverage of uncovr itself
void R_init_cov_xx(DllInfo *dll) {
  cov_init_altrep(dll);
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

void R_init_uncovr(DllInfo *dll) {
  cov_init_altrep(dll);
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

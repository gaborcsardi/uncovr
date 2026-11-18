#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP cov_make_counter(SEXP len);
SEXP cov_get_counts(SEXP counter);

#ifdef COV_BUILD_SAFE

#ifndef ENVFLAGS
struct sxpinfo_struct {
    SEXPTYPE type      :  TYPE_BITS;
                            /* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
                            * -> warning: `type' is narrower than values
                            *              of its type
                            * when SEXPTYPE was an enum */
    unsigned int scalar:  1;
    unsigned int obj   :  1;
    unsigned int alt   :  1;
    unsigned int gp    : 16;
};
struct SEXPREC {
    struct sxpinfo_struct sxpinfo;
};
#define ENVFLAGS(x) ((x)->sxpinfo.gp)
#define SET_ENVFLAGS(x,v) (((x)->sxpinfo.gp)=(v))
#endif

#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define LOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) | FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))
SEXP cov_lock_base(void) {
  LOCK_FRAME(R_BaseEnv);
  return Rf_ScalarLogical(1);
}
SEXP cov_unlock_base(void) {
  UNLOCK_FRAME(R_BaseEnv);
  return Rf_ScalarLogical(1);
}
#else
SEXP cov_lock_base(void) {
  return Rf_ScalarLogical(0);
}
SEXP cov_unlock_base(void) {
  return Rf_ScalarLogical(0);
}
#endif

void cov_init_altrep(DllInfo *dll);

#define CALLDEF(name, n) \
  { #name, (DL_FUNC)&name, n }

static const R_CallMethodDef callMethods[]  = {
  CALLDEF(cov_make_counter, 1),
  CALLDEF(cov_get_counts, 1),
  CALLDEF(cov_lock_base, 0),
  CALLDEF(cov_unlock_base, 0),
  { NULL, NULL, 0 }
};

void R_init_uncovr(DllInfo *dll) {
  cov_init_altrep(dll);
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

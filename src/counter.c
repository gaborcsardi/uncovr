#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Altrep.h>

R_altrep_class_t cov_counter_t;

SEXP cov_make_counter(SEXP len) {
  R_xlen_t clen = INTEGER(len)[0];
  SEXP v = Rf_allocVector(INTSXP, clen);
  memset(INTEGER(v), 0, sizeof(int) * clen);
  SEXP cntr = R_new_altrep(cov_counter_t, len, v);
  return cntr;
}

R_xlen_t cov_counter_Length(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  return INTEGER(data1)[0];
}

void* cov_counter_DataPtr(SEXP x, Rboolean writeable) {
  SEXP data2 = R_altrep_data2(x);
  return (void*) INTEGER(data2);
}

int cov_counter_Elt(SEXP x, R_xlen_t i) {
  SEXP data2 = R_altrep_data2(x);
  return INTEGER(data2)[i]++;
}

SEXP cov_get_counts(SEXP counter) {
  SEXP data2 = R_altrep_data2(counter);
  return Rf_duplicate(data2);
}

void cov_init_altrep(DllInfo *dll) {
  cov_counter_t = R_make_altinteger_class("cov_counter_t", "uncovr", dll);
  R_set_altrep_Length_method(cov_counter_t, cov_counter_Length);
  R_set_altvec_Dataptr_method(cov_counter_t, cov_counter_DataPtr);
  R_set_altinteger_Elt_method(cov_counter_t, cov_counter_Elt);
}

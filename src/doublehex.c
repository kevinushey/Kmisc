#include <R.h>
#include <Rinternals.h>

// [[export]]
SEXP double2hex(SEXP x) {
  unsigned long *xx = (unsigned long*) REAL(x);
  Rprintf("%02llX", *xx);
  return R_NilValue;
}

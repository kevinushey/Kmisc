#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[export]]
SEXP copy(SEXP x_) {
  SEXP x = PROTECT( duplicate(x_) );
  UNPROTECT(1);
  return x;
}

#undef USE_RINTERNALS

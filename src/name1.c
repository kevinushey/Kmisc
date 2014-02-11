#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[register]]
SEXP setnamed(SEXP x, SEXP i) {
  SET_NAMED(x, INTEGER(i)[0]);
  return R_NilValue;
}

#undef USE_RINTERNALS

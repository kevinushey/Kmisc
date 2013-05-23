#include <Rcpp.h>

SEXP f(SEXP x) {
  int len = Rf_length(x);
  for( int i=0; i < len; ++i ) {
    INTEGER(x)[i] = INTEGER(x)[i] + 1;
  }
  return R_NilValue;
}

RcppExport SEXP do_stuff( SEXP x ) {
  (*f)(x);
  return R_NilValue;
}

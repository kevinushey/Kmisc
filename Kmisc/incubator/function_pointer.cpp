#include <Rcpp.h>

void f(SEXP x) {
  int len = Rf_length(x);
  for( int i=0; i < len; ++i ) {
    INTEGER(x)[i] = INTEGER(x)[i] + 1;
  }
}

RcppExport SEXP do_stuff( SEXP x, void (*f)(SEXP x) ) {
  f(x);
  return R_NilValue;
}

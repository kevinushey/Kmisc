#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
IntegerVector do_fast_table( const Vector<RTYPE>& x ) {
  return table(x);
}

// [[Rcpp::export]]
SEXP fast_table( SEXP x ) {
  switch( TYPEOF(x) ) {
  case INTSXP: return do_fast_table<INTSXP>(x);
  case REALSXP: return do_fast_table<REALSXP>(x);
  case STRSXP: return do_fast_table<STRSXP>(x);
  default: {
    stop("unsupported RTYPE");
    return R_NilValue;
  }
  }
}
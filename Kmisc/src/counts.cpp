#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
inline
IntegerVector do_counts( const Vector<RTYPE>& x ) {
  return table(x);
}

// [[Rcpp::export]]
SEXP counts( SEXP x ) {
  switch( TYPEOF(x) ) {
  case INTSXP: return do_counts<INTSXP>(x);
  case REALSXP: return do_counts<REALSXP>(x);
  case STRSXP: return do_counts<STRSXP>(x);
  case LGLSXP: return do_counts<LGLSXP>(x);
  default: {
    Rf_error("'x' is of invalid type '%s'", Rf_type2char( TYPEOF(x) ));
    return R_NilValue;
  }
  }
}

#include <Rcpp.h>
using namespace Rcpp;

template <class T>
IntegerMatrix do_matches(List x) {
  int n = x.size();
  IntegerMatrix output(n, n);
  
  for (int i=0; i < n; ++i) {
    for (int j=0; j < n; ++j) {
      int tmp = sum( !is_na( match( as<T>(x[i]), as<T>(x[j]) ) ) );
      output(i, j) = (int) tmp;
    }
  }
  
  return output;
  
}

// [[Rcpp::export]]
IntegerMatrix matches(List x) {
  switch( TYPEOF(x[0]) ) {
    case INTSXP: return do_matches<IntegerVector>(x);
    case REALSXP: return do_matches<NumericVector>(x);
    case STRSXP: return do_matches<CharacterVector>(x);
    case LGLSXP: return do_matches<IntegerVector>(x);
    default: {
      stop("invalid SEXP type");
      return R_NilValue;
    }
    }
}

#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
Vector<RTYPE> do_swap( const Vector<RTYPE> vec, const Vector<RTYPE>& from, const Vector<RTYPE>& to ) {
  IntegerVector matches = match(vec, from) - 1;
  int n = vec.size();
  Vector<RTYPE> out(n);
  for( int i=0; i < n; ++i ) {
    // Rcout << "matches[" << i << "] is: " << matches[i] << std::endl;
    if( !IntegerVector::is_na( matches[i] ) ) {
      // Rcout << "\tmatches[" << i << "] is not NA" << std::endl;
      out[i] = to[ matches[i] ];
    } else {
      // Rcout << "\tmatches[" << i << "] is NA" << std::endl;
      out[i] = vec[i];
    }
  }
  return out;
}

// [[Rcpp::export]]
SEXP swap( SEXP vec, SEXP from, SEXP to ) {
  
  switch( TYPEOF(to) ) {
    case INTSXP: return do_swap<INTSXP>(vec, from, to);
    case REALSXP: return do_swap<REALSXP>(vec, from, to);
    case STRSXP: return do_swap<STRSXP>(vec, from, to);
    //case LGLSXP: return do_swap<LGLSXP>(x, y);
    default: stop("incompatible RTYPE");
  }
  
  return R_NilValue;
  
}

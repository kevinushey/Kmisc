#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
IntegerVector fast_factor_template( const Vector<RTYPE>& x ) {
  
  Vector<RTYPE> sorted = sort_unique(x);
  IntegerVector out = match( x, sorted );
  
  // handle NAs
  if( Vector<RTYPE>::is_na( *sorted.begin() ) ) {
    
    out = out - 1;
    // we replace all 0's with NAs in the output
    for( IntegerVector::iterator it = out.begin(); it != out.end(); ++it ) {
      if( (*it) == 0 ) {
        (*it) = NA_INTEGER;
      }
    }
    
    // we remove the first element from sorted, since it acts as levels
    Vector<RTYPE> levels( sorted.begin()+1, sorted.end() );
    out.attr("levels") = as<CharacterVector>( levels );
    
  } else {
    out.attr("levels") = as<CharacterVector>( sorted );
  }
  
  out.attr("class") = "factor";
  return out;
  
}

// [[Rcpp::export]]
SEXP fast_factor( SEXP x ) {
  int type = TYPEOF(x);
  switch( type ) {
    case INTSXP: return fast_factor_template<INTSXP>( x );
    case REALSXP: return fast_factor_template<REALSXP>( x );
    case STRSXP: return fast_factor_template<STRSXP>( x );
    case LGLSXP: return fast_factor_template<INTSXP>( x );
  }
  stop("fast_factor not implemented for this object type");
  return R_NilValue;
}

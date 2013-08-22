#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
IntegerVector fast_factor_template( const Vector<RTYPE>& x, SEXP levels, bool isNull ) {

  Vector<RTYPE> sorted;
  if (isNull) {
    sorted = sort_unique(x);
  } else {
    sorted = Vector<RTYPE>(levels);
  }
  
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
SEXP fast_factor( SEXP x, SEXP levels ) {
  int type = TYPEOF(x);
  if (!Rf_isNull(levels) && TYPEOF(levels) != type) {
    levels = Rf_coerceVector(levels, type);
  }
  bool isNull = Rf_isNull(levels);
	switch( type ) {
	case INTSXP: return fast_factor_template<INTSXP>(x, levels, isNull);
	case REALSXP: return fast_factor_template<REALSXP>(x, levels, isNull);
	case STRSXP: return fast_factor_template<STRSXP>(x, levels, isNull);
	case LGLSXP: return fast_factor_template<INTSXP>(x, levels, isNull);
	}
	Rf_error("argument is of incompatible type '%s'", Rf_type2char( TYPEOF(x) ));
	return R_NilValue;
}

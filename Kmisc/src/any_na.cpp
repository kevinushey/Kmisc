#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
inline bool do_any_NA( const Vector<RTYPE>& x ) {
	return is_true( any( is_na(x) ) );
}

// [[Rcpp::export]]
bool any_NA( SEXP x ) {
	switch( TYPEOF(x) ) {
	case INTSXP: return do_any_NA<INTSXP>(x);
	case REALSXP: return do_any_NA<REALSXP>(x);
	case LGLSXP: return do_any_NA<LGLSXP>(x);
	case STRSXP: return do_any_NA<STRSXP>(x);
	case RAWSXP: return do_any_NA<RAWSXP>(x);
	default:
		stop("Incompatible RTYPE");
		return false;
	}
}

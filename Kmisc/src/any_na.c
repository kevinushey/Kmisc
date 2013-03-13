#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#define HANDLE_CASE( RTYPE, CTYPE, ACCESSOR ) \
	case RTYPE: { \
		CTYPE* ptr = ACCESSOR(x); \
		for( int i=0; i < len; ++i ) { \
			if( ISNA( ptr[i] ) || ISNAN( ptr[i] ) ) { \
				LOGICAL(out)[0] = TRUE; \
				UNPROTECT(1); \
				return out; \
			} \
		} \
		LOGICAL(out)[0] = FALSE; \
		UNPROTECT(1); \
		return out; \
	} \

SEXP any_na( SEXP x ) {
	SEXP out;
	PROTECT(out = NEW_LOGICAL(1));
	int len = Rf_length(x);
	switch( TYPEOF(x) ) {
	HANDLE_CASE( INTSXP, int, INTEGER )
	HANDLE_CASE( REALSXP, double, REAL )
	HANDLE_CASE( LGLSXP, int, LOGICAL )
	case STRSXP: {
		for( int i=0; i < len; ++i ) {
			if( STRING_ELT(x, i) == NA_STRING ) {
				LOGICAL(out)[0] = TRUE;
				UNPROTECT(1);
				return out;
			}
		}
		LOGICAL(out)[0] = FALSE;
		UNPROTECT(1);
		return out;
	}
	}
	Rf_error("Unrecognized RTYPE");
}

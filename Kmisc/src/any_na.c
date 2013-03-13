#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP any_na( SEXP x ) {
	SEXP out;
	PROTECT(out = NEW_LOGICAL(1));
	int len = Rf_length(x);
	switch( TYPEOF(x) ) {
	case REALSXP: {
		double* ptr = REAL(x);
		for( int i=0; i < len; ++i ) {
			if( ISNA( ptr[i] ) || ISNAN( ptr[i] ) ) {
				LOGICAL(out)[0] = TRUE;
				UNPROTECT(1);
				return out;
			}
		}
		LOGICAL(out)[0] = FALSE;
		UNPROTECT(1);
		return out;
	}
	case INTSXP: {
		int* ptr = INTEGER(x);
		for( int i=0; i < len; ++i ) {
			if( ptr[i] == NA_INTEGER ) {
				LOGICAL(out)[0] = TRUE;
				UNPROTECT(1);
				return out;
			}
		}
		LOGICAL(out)[0] = FALSE;
		UNPROTECT(1);
		return out;
	}
	case LGLSXP: {
		int* ptr = LOGICAL(x);
		for( int i=0; i < len; ++i ) {
			if( ptr[i] == NA_LOGICAL ) {
				LOGICAL(out)[0] = TRUE;
				UNPROTECT(1);
				return out;
			}
		}
		LOGICAL(out)[0] = FALSE;
		UNPROTECT(1);
		return out;
	}
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
	return x;
}

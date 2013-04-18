#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>

SEXP list_to_dataframe( SEXP x_ ) {

	if( TYPEOF(x_) != VECSXP )
		Rf_error("x must be a list");

	SEXP x = PROTECT( duplicate(x_) );

	int m = length(x);
	int n = length( VECTOR_ELT(x, 0) );
	for( int i=1; i < m; ++i ) {
		if( length( VECTOR_ELT(x, i) ) != n ) {
			Rf_error("not all columns are of equal length");
		}
	}

	SEXP row_names = PROTECT( allocVector( INTSXP, n ) );
	for( int i=0; i < n; ++i )
		INTEGER(row_names)[i] = i+1;

	setAttrib(x, R_ClassSymbol, mkString("data.frame"));
	setAttrib(x, R_RowNamesSymbol, row_names);
	UNPROTECT(2);
	return x;

}

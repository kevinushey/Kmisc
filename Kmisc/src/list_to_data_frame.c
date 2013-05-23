#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>

SEXP list_to_dataframe( SEXP x_, SEXP in_place ) {

  SEXP x;
  int unprotect_num = 1;
  
  if( TYPEOF(in_place) != LGLSXP || length(in_place) > 1 ) {
    error("'in_place' must be a logical vector of length 1; type is '%s'", type2char(TYPEOF(in_place)));
  }
  
	if( TYPEOF(x_) != VECSXP )
		error("argument must be a list; type is '%s'", type2char(TYPEOF(x_)));
  
  if( LOGICAL(in_place)[0] ) {
    x = x_;
  } else {
    x = PROTECT( duplicate(x_) );
    ++unprotect_num;
  }

	int m = length(x);
	int n = length( VECTOR_ELT(x, 0) );
	for( int i=1; i < m; ++i ) {
		if( length( VECTOR_ELT(x, i) ) != n ) {
			error("not all columns are of equal length");
		}
	}

	SEXP row_names = PROTECT( allocVector( INTSXP, n ) );
	for( int i=0; i < n; ++i )
		INTEGER(row_names)[i] = i+1;

	setAttrib(x, R_ClassSymbol, mkString("data.frame"));
	setAttrib(x, R_RowNamesSymbol, row_names);
	UNPROTECT( unprotect_num );
	return x;

}

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[register]]
SEXP charlist_transpose_to_df( SEXP x, SEXP names ) {
  
  if( TYPEOF(x) != VECSXP ) {
    error("argument must be a list; type is '%s'", type2char( TYPEOF(x)));
  }
  
  int out_nRow = length(x);
	int out_nCol = length( VECTOR_ELT(x, 0) );
  for (int i=0; i < out_nRow; ++i) {
    if (length( VECTOR_ELT(x, i)) != out_nCol) {
      error("each column of 'x' must be of equal length");
    }
  }

	SEXP out = PROTECT( allocVector( VECSXP, out_nCol ) );

	for( int j=0; j < out_nCol; ++j ) {
		SEXP tmp = PROTECT( allocVector( STRSXP, out_nRow ) );
		for( int i=0; i < out_nRow; ++i ) {
			SET_STRING_ELT( tmp, i, STRING_ELT( VECTOR_ELT( x, i ), j ) );
		}
		SET_VECTOR_ELT( out, j, tmp );
		UNPROTECT(1);
	}

	SEXP row_names = PROTECT( allocVector( INTSXP, out_nRow ) );
	int* row_names_ptr = INTEGER(row_names);
	for( int i=0; i < out_nRow; ++i ) {
		row_names_ptr[i] = i+1;
	}

	setAttrib(out, R_ClassSymbol, mkString("data.frame"));
	setAttrib(out, R_RowNamesSymbol, row_names);
  
  // make the names
#define m out_nCol
  if (isNull(names)) {
    SEXP nm = PROTECT( allocVector(STRSXP, out_nCol) );
    char str[ (int) log10(m) + 3];
    for (int i = 0; i < m; ++i) {
			sprintf(str, "%s%i", "V", i + 1);
			SET_STRING_ELT(nm, i, mkChar(str));
		}
		setAttrib(out, R_NamesSymbol, nm);
    UNPROTECT(1);
	} else {
    setAttrib(out, R_NamesSymbol, names);
	}
#undef m
  

	UNPROTECT(2);
	return out;
}

#undef USE_RINTERNALS

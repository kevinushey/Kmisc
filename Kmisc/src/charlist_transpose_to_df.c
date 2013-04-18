#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

SEXP charlist_transpose_to_df( SEXP x ) {

	int out_nRow = length(x);
	int out_nCol = length( VECTOR_ELT(x, 0) );

	SEXP out = PROTECT( allocVector( VECSXP, out_nCol ) );

	for( int j=0; j < out_nCol; ++j ) {
		SEXP tmp = PROTECT( allocVector( STRSXP, out_nRow ) );
		//SEXP* tmp_ptr = STRING_PTR(tmp);
		for( int i=0; i < out_nRow; ++i ) {
			//tmp_ptr[i] = x_ptrs[i][j];
			//tmp_ptr[i] = STRING_ELT( VECTOR_ELT( x, i ), j );
			SET_STRING_ELT( tmp, i, STRING_ELT( VECTOR_ELT( x, i ), j ) );
		}
		SET_VECTOR_ELT( out, j, tmp );
		UNPROTECT(1);
	}

	SEXP row_names = PROTECT( allocVector( INTSXP, out_nRow ) );
	int* row_names_ptr = INTEGER(row_names);
	for( int i=0; i < out_nRow; ++i )
		row_names_ptr[i] = i+1;

	setAttrib(out, R_ClassSymbol, mkString("data.frame"));
	setAttrib(out, R_RowNamesSymbol, row_names);

	UNPROTECT(2);
	return out;
}

#undef USE_RINTERNALS

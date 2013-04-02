#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

SEXP rep_row_names( SEXP x, int times ) {
	SEXP out;
	int len = length(x);
	int counter = 0;
	PROTECT( out = allocVector( STRSXP, len*times ) );
	SEXP* x_ptr = STRING_PTR(x);
	SEXP* out_ptr = STRING_PTR(out);
	for( int i=0; i < times; ++i ) {
		for( int j=0; j < len; ++j ) {
			out_ptr[counter] = x_ptr[j];
			//SET_STRING_ELT( out, counter, STRING_ELT(x, j) );
			++counter;
		}
	}
	UNPROTECT(1);
	return out;
}
SEXP rep_col_names( SEXP x, int each ) {
	SEXP out;
	int len = length(x);
	PROTECT( out = allocVector( STRSXP, len*each ) );
	int counter=0;
	SEXP* ptr = STRING_PTR(x);
	SEXP* out_ptr = STRING_PTR(out);
	for( int i=0; i < len; ++i ) {
		for( int j=0; j < each; j++ ) {
			out_ptr[counter] = ptr[i];
			//SET_STRING_ELT( out, counter, ptr[i] );
			++counter;
		}
	}
	UNPROTECT(1);
	return out;
}

SEXP matrix_to_vector( SEXP x, int size ) {

	SEXP out;
	switch( TYPEOF(x) ) {
	case INTSXP: {
		PROTECT( out = allocVector(INTSXP, size) );
		int* mat_ptr = INTEGER(x);
		int* out_ptr = INTEGER(out);
		for( int i=0; i < size; ++i ) {
			out_ptr[i] = mat_ptr[i];
		}
		UNPROTECT(1);
		return out;
	}
	case REALSXP: {
		PROTECT( out = allocVector(REALSXP, size) );
		double* mat_ptr = REAL(x);
		double* out_ptr = REAL(out);
		for( int i=0; i < size; ++i ) {
			out_ptr[i] = mat_ptr[i];
		}
		UNPROTECT(1);
		return out;
	}
	case LGLSXP: {
		PROTECT( out = allocVector(LGLSXP, size) );
		int* mat_ptr = LOGICAL(x);
		int* out_ptr = LOGICAL(out);
		for( int i=0; i < size; ++i ) {
			out_ptr[i] = mat_ptr[i];
		}
		UNPROTECT(1);
		return out;
	}
	case STRSXP: {
		PROTECT( out = allocVector( STRSXP, size ) );
		SEXP* mat_ptr = STRING_PTR(x);
		SEXP* out_ptr = STRING_PTR(out);
		for( int i=0; i < size; ++i ) {
			out_ptr[i] = mat_ptr[i];
		}
		UNPROTECT(1);
		return out;
	}
	default: {
		return R_NilValue;
	}
	}

}

SEXP melt_matrix( SEXP x ) {

	SEXP row, col, out, out_row_names;

	int nRow = nrows(x);
	int nCol = ncols(x);
	int out_nRow = nRow*nCol;
	int out_nCol = 3;

	// the output will be a 3 column data.frame
	// 1: repeated row names / indexes
	// 2: repeated col names / indexes
	// 3: values

	SEXP row_names, col_names;
	const char* row_names_char;
	const char* col_names_char;
	GetMatrixDimnames(x, &row_names, &col_names, &row_names_char, &col_names_char);

	PROTECT( out = allocVector( VECSXP, 3 ) );
	int counter;

	// row indices
	if( isNull(row_names) ) {
		PROTECT( row = allocVector( INTSXP, out_nRow ) );
		int* row_ptr = INTEGER(row);
		counter = 0;
		for( int i=0; i < nCol; ++i ) {
			for( int j=0; j < nRow; ++j ) {
				row_ptr[counter] = j+1;
				++counter;
			}
		}
	} else {
		PROTECT( row = rep_row_names(row_names, nCol) );
	}

	// col indices
	if( isNull(col_names) ) {
		PROTECT( col = allocVector( INTSXP, out_nRow ) );
		int* col_ptr = INTEGER(col);
		counter = 0;
		for( int i=0; i < nCol; ++i ) {
			for( int j=0; j < nRow; ++j ) {
				col_ptr[counter] = i+1;
				++counter;
			}
		}
	} else {
		PROTECT( col = rep_col_names(col_names, nRow) );
	}

	// dim_names is a list; 1st entry is row names, 2nd is col names
	SET_VECTOR_ELT( out, 0, row );
	SET_VECTOR_ELT( out, 1, col );
	SET_VECTOR_ELT( out, 2, matrix_to_vector(x, out_nRow) );

	// set row names
	PROTECT( out_row_names = allocVector( INTSXP, out_nRow) );
	int* row_names_ptr = INTEGER(out_row_names);
	for( int i=0; i < out_nRow; ++i ) {
		row_names_ptr[i] = i+1;
	}
	setAttrib( out, R_RowNamesSymbol, out_row_names );
	UNPROTECT(1);

	// set class
	setAttrib( out, R_ClassSymbol, mkString("data.frame") );

	// set names
	SEXP names;
	PROTECT( names = allocVector( STRSXP, out_nCol ) );
	SET_STRING_ELT( names, 0, mkChar("row") );
	SET_STRING_ELT( names, 1, mkChar("col") );
	SET_STRING_ELT( names, 2, mkChar("value") );
	setAttrib( out, R_NamesSymbol, names );
	UNPROTECT(1);

	// unprotect the rest of the stuff from earlier
	UNPROTECT(3);
	return out;

}

#undef USE_RINTERNALS

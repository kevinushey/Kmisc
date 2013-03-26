#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

SEXP rep_each_char( SEXP x, int each ) {

	SEXP out;
	int len = length(x);
	PROTECT( out = allocVector( STRSXP, len*each ) );
	int counter=0;
	SEXP* ptr = STRING_PTR(x);
	SEXP* out_ptr = STRING_PTR(out);
	for( int i=0; i < len; ++i ) {
		for( int j=0; j < each; ++j ) {
			out_ptr[counter] = ptr[i];
			//SET_STRING_ELT( out, counter, ptr[i] );
			++counter;
		}
	}
	UNPROTECT(1);
	return out;
}

#define HANDLE_CASE( RTYPE, CTYPE, ACCESSOR ) \
		case RTYPE: { \
			PROTECT( out = allocVector( RTYPE, len*times ) ); \
			CTYPE* ptr = ACCESSOR(x); \
			CTYPE* out_ptr = ACCESSOR(out); \
			for( int i=0; i < times; ++i ) { \
				for( int j=0; j < len; ++j ) { \
					out_ptr[counter] = ptr[j]; \
					++counter; \
				} \
			} \
			UNPROTECT(1); \
			return out; \
		} \

SEXP stack_vector( SEXP x, int times ) {
	SEXP out;
	int len = length(x);
	int counter = 0;
	switch( TYPEOF(x) ) {
	HANDLE_CASE( INTSXP, int, INTEGER );
	HANDLE_CASE( REALSXP, double, REAL );
	HANDLE_CASE( LGLSXP, int, LOGICAL );
	HANDLE_CASE( STRSXP, SEXP, STRING_PTR );
	}

	error("Stacking not implemented for vector of this RTYPE");
	return R_NilValue;
}
#undef HANDLE_CASE

SEXP melt_dataframe( SEXP x_stack, SEXP x_rep ) {

	int nColStack = length(x_stack);
	int nColRep = length(x_rep);
	int nRow = length( VECTOR_ELT(x_stack, 0) );
	int out_nRow = nRow * nColRep;
	int out_nCol = nColStack + 2;

	SEXP out;
	PROTECT( out = allocVector( VECSXP, out_nCol ) );

	// populate the value array
	SEXP value_SEXP;

#define HANDLE_CASE( RTYPE, CTYPE, ACCESSOR ) \
		case RTYPE: { \
			PROTECT( value_SEXP = allocVector( RTYPE, value_len ) ); \
			int counter = 0; \
			CTYPE* ptr_val = ACCESSOR( value_SEXP ); \
			for( int i=0; i < nColRep; ++i ) { \
				CTYPE* ptr = ACCESSOR( VECTOR_ELT( x_rep, i ) ); \
				for( int j=0; j < nRow; ++j ) { \
					ptr_val[counter] = ptr[j]; \
					++counter; \
				} \
			} \
			break; \
		} \


	int value_len = nColRep * nRow;
	int value_type = TYPEOF( VECTOR_ELT( x_rep, 0 ) );
	switch( value_type ) {
	HANDLE_CASE( INTSXP, int, INTEGER );
	HANDLE_CASE( REALSXP, double, REAL );
	HANDLE_CASE( LGLSXP, int, LOGICAL );
	case STRSXP: {
		int counter = 0;
		PROTECT( value_SEXP = allocVector( STRSXP, value_len ) );
		for( int i=0; i < nColRep; ++i ) {
			SEXP curr_str_vec = VECTOR_ELT( x_rep, i );
			SEXP* value_SEXP_ptr = STRING_PTR( value_SEXP );
			SEXP* curr_str_vec_ptr = STRING_PTR(curr_str_vec);
			for( int j=0; j < nRow; ++j ) {
				value_SEXP_ptr[counter] = curr_str_vec_ptr[j];
				//SET_STRING_ELT( value_SEXP, counter, STRING_ELT( curr_str_vec, j ) );
				++counter;
			}
		}
		break;
	}
	default:
		error("Unsupported RTYPE encountered");
	}
#undef HANDLE_CASE

	// generate the id variables, and assign them on generation
	for( int i=0; i < nColStack; ++i ) {
		SET_VECTOR_ELT( out, i, stack_vector( VECTOR_ELT( x_stack, i ), nColRep ) );
	}

	// assign the names, values
	SET_VECTOR_ELT( out, nColStack, rep_each_char( getAttrib( x_rep, R_NamesSymbol ), nRow ) );
	SET_VECTOR_ELT( out, nColStack+1, value_SEXP );
	UNPROTECT(1); // value_SEXP

	// set the row names
	SEXP row_names;
	PROTECT( row_names = allocVector(INTSXP, out_nRow) );
	int* row_names_ptr = INTEGER(row_names);
	for( int i=0; i < out_nRow; ++i ) {
		row_names_ptr[i] = i+1;
	}
	setAttrib( out, R_RowNamesSymbol, row_names );
	UNPROTECT(1);

	// set the class to data.frame
	setAttrib( out, R_ClassSymbol, mkString("data.frame") );

	// set the names
	SEXP names = getAttrib( x_stack, R_NamesSymbol );
	SEXP names_out;
	PROTECT( names_out = allocVector( STRSXP, out_nCol ) );
	for( int i=0; i < nColStack; ++i ) {
		SET_STRING_ELT( names_out, i, STRING_ELT( names, i ) );
	}
	SET_STRING_ELT( names_out, nColStack, mkChar("names") );
	SET_STRING_ELT( names_out, nColStack+1, mkChar("value") );
	setAttrib( out, R_NamesSymbol, names_out );
	UNPROTECT(1);

	UNPROTECT(1); // out
	return out;

}

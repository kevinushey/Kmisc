#include <R.h>
#include <Rinternals.h>

SEXP rep_each_char( SEXP x, int each ) {

	SEXP out;
	int len = Rf_length(x);
	PROTECT( out = Rf_allocVector( STRSXP, len*each ) );
	int counter=0;
	for( int i=0; i < len; ++i ) {
		for( int j=0; j < each; j++ ) {
			SET_STRING_ELT( out, counter, STRING_ELT( x, i ) );
			++counter;
		}
	}
	UNPROTECT(1);
	return out;
}

#define HANDLE_CASE( RTYPE, CTYPE, ACCESSOR ) \
case RTYPE: { \
	PROTECT( out = Rf_allocVector( RTYPE, len*times ) ); \
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
	int len = Rf_length(x);
	int counter = 0;
	switch( TYPEOF(x) ) {
	HANDLE_CASE( INTSXP, int, INTEGER );
	HANDLE_CASE( REALSXP, double, REAL );
	HANDLE_CASE( LGLSXP, int, LOGICAL );
	case STRSXP: {
		PROTECT( out = Rf_allocVector( STRSXP, len*times ) );
		for( int i=0; i < times; ++i ) {
			for( int j=0; j < len; ++j ) {
				SET_STRING_ELT( out, counter, STRING_ELT(x, j) );
				++counter;
			}
		}
		UNPROTECT(1);
		return out;
	}
	}

	Rf_error("Stacking not implemented for vector of this RTYPE");
	return R_NilValue;
}
#undef HANDLE_CASE

SEXP melt_dataframe( SEXP x_stack, SEXP x_rep ) {

	int nColStack = Rf_length(x_stack);
	int nColRep = Rf_length(x_rep);
	int nRow = Rf_length( VECTOR_ELT(x_stack, 0) );

	SEXP out;
	PROTECT( out = Rf_allocVector( VECSXP, nColStack+2 ) );

	// populate the value array
	SEXP value_SEXP;

#define HANDLE_CASE( RTYPE, CTYPE, ACCESSOR ) \
	case RTYPE: { \
		PROTECT( value_SEXP = Rf_allocVector( RTYPE, value_len ) ); \
		int counter = 0; \
		for( int i=0; i < nColRep; ++i ) { \
			CTYPE* ptr = ACCESSOR( VECTOR_ELT( x_rep, i ) ); \
			CTYPE* ptr_val = ACCESSOR( value_SEXP ); \
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
		PROTECT( value_SEXP = Rf_allocVector( STRSXP, value_len ) );
		for( int i=0; i < nColRep; ++i ) {
			SEXP curr_str_vec = VECTOR_ELT( x_rep, i );
			for( int j=0; j < nRow; ++j ) {
				SET_STRING_ELT( value_SEXP, counter, STRING_ELT( curr_str_vec, j ) );
				++counter;
			}
		}
		break;
	}
	default:
		Rf_error("Unsupported RTYPE encountered");
	}
#undef HANDLE_CASE

	// generate the id variables, and assign them on generation
	for( int i=0; i < nColStack; ++i ) {
		SET_VECTOR_ELT( out, i, stack_vector( VECTOR_ELT( x_stack, i ), nColRep ) );
	}

	// assign the names, values
	SET_VECTOR_ELT( out, nColStack, rep_each_char( Rf_getAttrib( x_rep, Rf_install("names") ), nRow ) );
	SET_VECTOR_ELT( out, nColStack+1, value_SEXP );

	UNPROTECT(2);

	return out;

}

#include <R.h>
#include <Rdefines.h>

SEXP rep_each( SEXP x, int each ) {
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

SEXP stack_vector( SEXP x, int times ) {
	SEXP out;
	int len = Rf_length(x);
	int counter = 0;
	switch( TYPEOF(x) ) {
	case INTSXP: {
		PROTECT( out = Rf_allocVector( INTSXP, len*times ) );
		int* ptr_int = INTEGER(x);
		int* out_ptr = INTEGER(out);
		for( int i=0; i < times; ++i ) {
			for( int j=0; j < len; ++j ) {
				out_ptr[counter] = ptr_int[j];
				++counter;
			}
		}
		UNPROTECT(1);
		return out;
	}
	case REALSXP: {
		PROTECT( out = Rf_allocVector( REALSXP, len*times ) );
		double* ptr_real = REAL(x);
		double* out_ptr = REAL(out);
		for( int i=0; i < times; ++i ) {
			for( int j=0; j < len; ++j ) {
				out_ptr[counter] = ptr_real[j];
				++counter;
			}
		}
		UNPROTECT(1);
		return out;
	}
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
}


SEXP melt_dataframe( SEXP x_stack, SEXP x_rep ) {

	int nColStack = Rf_length(x_stack);
	int nColRep = Rf_length(x_rep);
	int nRow = Rf_length( VECTOR_ELT(x_stack, 0) );

	SEXP out;
	PROTECT( out = Rf_allocVector( VECSXP, nColStack+2 ) );

	int value_len = nColRep * nRow;

	// populate the value array
	SEXP value_SEXP;
	PROTECT( value_SEXP = Rf_allocVector( REALSXP, value_len ) );
	int counter = 0;
	for( int i=0; i < nColRep; ++i ) {
		double* ptr = REAL( VECTOR_ELT( x_rep, i ) );
		double* ptr_val = REAL( value_SEXP );
		for( int j=0; j < nRow; ++j ) {
			ptr_val[counter] = ptr[j];
			++counter;
		}
	}

	// generate the id variables, and assign them on generation
	for( int i=0; i < nColStack; ++i ) {
		SET_VECTOR_ELT( out, i, stack_vector( VECTOR_ELT( x_stack, i ), nColRep ) );
	}

	// assign the names, values
	SET_VECTOR_ELT( out, nColStack, rep_each( Rf_getAttrib( x_rep, Rf_install("names") ), nRow ) );
	SET_VECTOR_ELT( out, nColStack+1, value_SEXP );
	UNPROTECT(2);
  
  // set the attributes of out so it's reinterpretted as a df
  

	return out;

}

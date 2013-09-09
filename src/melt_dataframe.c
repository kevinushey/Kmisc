#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

// a function that operates like R's 'rep(..., each=each)',
// but only works for characters
SEXP rep_each_char( SEXP x, SEXP id_ind_, int each ) {

	SEXP out;
  int* id_ind = INTEGER(id_ind_);
	int len = length(id_ind_);
	PROTECT( out = allocVector( STRSXP, len*each ) );
	int counter=0;
	SEXP* ptr = STRING_PTR(x);
	SEXP* out_ptr = STRING_PTR(out);
	for( int i=0; i < len; ++i ) {
		for( int j=0; j < each; ++j ) {
			out_ptr[counter] = ptr[ id_ind[i] ];
			++counter;
		}
	}
	UNPROTECT(1);
	return out;
}

// some macro definitions used for repeating a vector 
// n times

// simply using memcpy with strings is dangerous
#define HANDLE_CASE_STRING \
  	case STRSXP: { \
    int counter = 0; \
		PROTECT( out = allocVector( STRSXP, len*times ) ); \
		SEXP* ptr = STRING_PTR(x); \
		SEXP* out_ptr = STRING_PTR(out); \
		for( int i=0; i < times; ++i ) { \
			for( int j=0; j < len; ++j ) { \
				out_ptr[counter] = ptr[j]; \
				++counter; \
			} \
		} \
		UNPROTECT(1); \
		return out; \
		} \

#define HANDLE_CASE(RTYPE, CTYPE) \
  case RTYPE: { \
  char sz = sizeof(CTYPE); \
  PROTECT(out = allocVector(RTYPE, len*times)); \
  for (int i=0; i < times; ++i) { \
    memcpy( \
      (char*) DATAPTR(out) + (i*len*sz), \
      (char*) DATAPTR(x), \
      len*sz \
    ); \
  } \
  UNPROTECT(1); \
  return out; \
  } \

SEXP stack_vector( SEXP x, int times ) {
	SEXP out;
	int len = length(x);
	switch( TYPEOF(x) ) {
	HANDLE_CASE( INTSXP, int );
	HANDLE_CASE( REALSXP, double );
	HANDLE_CASE( LGLSXP, int );
	HANDLE_CASE_STRING;
	}
  
  // if we've reached here, we have an unhandled / incompatible SEXP type
	error("argument is of incompatible type '%s'", type2char(TYPEOF(x)));
	return R_NilValue;
}

#undef HANDLE_CASE

// checks if all values in a VECSXP x are of the same type
bool diff_types(SEXP x, SEXP val_ind_) {
  if (TYPEOF(x) != VECSXP) {
    error("Expected a VECSXP but got a '%s'", type2char(TYPEOF(x)));
  }
  int n = length(val_ind_);
  int* val_ind = INTEGER(val_ind_);
  char type = TYPEOF( VECTOR_ELT(x, val_ind[0]) );
  for (int i=1; i < n; ++i) {
    if (TYPEOF( VECTOR_ELT(x, val_ind[i]) ) != type) {
      return true;
    }
  }
  return false;
}

// get the largest type available within a vector
char max_type(SEXP x, SEXP ind_) {
  if (TYPEOF(x) != VECSXP) {
    error("Expected a VECSXP but got a '%s'", type2char(TYPEOF(x)));
  }
  int n = length(ind_);
  int* ind = INTEGER(ind_);
  char max_type = -1;
  char tmp = -1;
  for (int i=0; i < n; ++i) {
    // factors should mean we coerce to string
    if (isFactor(VECTOR_ELT(x, ind[i]))) {
      if (STRSXP > max_type) {
        max_type = STRSXP;
      }
    } else if ((tmp = TYPEOF( VECTOR_ELT(x, ind[i]) )) > max_type) {
      max_type = tmp;
    }
  }
  return max_type;
}

SEXP melt_dataframe( SEXP x, SEXP id_ind_, SEXP val_ind_, SEXP variable_name, SEXP value_name ) {
  
  if (length(x) == 0) {
    error("Can't melt a data.frame with 0 columns");
  }
  
  if (length(VECTOR_ELT(x, 0)) == 0) {
    error("Can't melt a data.frame with 0 rows");
  }
  
  int* id_ind = INTEGER(id_ind_);
  int* val_ind = INTEGER(val_ind_);
  
  int nColStack = length(id_ind_);
	int nColRep = length(val_ind_);
  
  int nRow = length( VECTOR_ELT(x, 0) );
	int out_nRow = nRow * nColRep;
	int out_nCol = nColStack + 2;
  
  char mt = max_type(x, val_ind_);
  if (mt > STRSXP) {
    error("Error: cannot melt data.frames w/ elements of type '%s'", CHAR(type2str(mt)));
  }
  
  if (diff_types(x, val_ind_)) {
    warning("Coercing type of 'value' variables to '%s'", CHAR(type2str(mt)));
  }
  
  SEXP out;
	PROTECT(out = allocVector( VECSXP, out_nCol ));

	// populate the value array
	SEXP value_SEXP;

#define HANDLE_CASE( RTYPE, CTYPE ) \
		case RTYPE: { \
      PROTECT( value_SEXP = allocVector( RTYPE, value_len ) ); \
      SEXP tmp; \
			for( int i=0; i < nColRep; ++i ) { \
        if (TYPEOF( VECTOR_ELT(x, val_ind[i]) ) != mt) { \
          tmp = PROTECT( coerceVector( VECTOR_ELT(x, val_ind[i]), mt ) ); \
        } else { \
          tmp = VECTOR_ELT(x, val_ind[i]); \
        } \
        memcpy( \
          (char*) DATAPTR(value_SEXP) + (i*nRow*sizeof(CTYPE)), \
          (char*) DATAPTR(tmp), \
          nRow * sizeof(CTYPE) \
        ); \
        if (TYPEOF( VECTOR_ELT(x, val_ind[i]) ) != mt) { \
          UNPROTECT(1); \
        } \
			} \
			break; \
		} \


	int value_len = nColRep * nRow;
	int value_type = mt;
  switch( value_type ) {
	HANDLE_CASE( INTSXP, int );
	HANDLE_CASE( REALSXP, double );
	HANDLE_CASE( LGLSXP, int );
	case STRSXP: {
    int counter = 0;
    SEXP* curr_str_vec_ptr;
    SEXP tmp;
		PROTECT( value_SEXP = allocVector( STRSXP, value_len ) );
		for( int i=0; i < nColRep; ++i ) {
#define curr_str_vec (VECTOR_ELT(x, val_ind[i]))
      if (TYPEOF(curr_str_vec) != STRSXP) {
        if (isFactor(curr_str_vec)) {
          PROTECT(tmp = asCharacterFactor(curr_str_vec));
        } else {
          PROTECT(tmp = coerceVector(curr_str_vec, STRSXP));
        }
        curr_str_vec_ptr = STRING_PTR(tmp);
      } else {
        curr_str_vec_ptr = STRING_PTR(curr_str_vec);
      }
#undef curr_str_vec
			SEXP* value_SEXP_ptr = STRING_PTR( value_SEXP );
			for( int j=0; j < nRow; ++j ) {
				value_SEXP_ptr[counter] = curr_str_vec_ptr[j];
				++counter;
			}
      if (TYPEOF( VECTOR_ELT(x, val_ind[i]) ) != mt) {
        UNPROTECT(1);
      }
		}
		break;
	}
	default:
		error("Unsupported RTYPE encountered");
	}
  
#undef HANDLE_CASE

	// generate the id variables, and assign them on generation
  // we need to convert factors if necessary
	for( int i=0; i < nColStack; ++i ) {
		SET_VECTOR_ELT( out, i, stack_vector( VECTOR_ELT( x, id_ind[i] ), nColRep ));
    if (isFactor( VECTOR_ELT(x, id_ind[i]) )) {
      setAttrib( VECTOR_ELT(out, i), R_ClassSymbol, mkString("factor") );
      setAttrib( VECTOR_ELT(out, i), R_LevelsSymbol, getAttrib( VECTOR_ELT(x, id_ind[i]), R_LevelsSymbol ) );
    }
	}

	// assign the names, values
	SET_VECTOR_ELT( out, nColStack, rep_each_char( getAttrib( x, R_NamesSymbol ), val_ind_, nRow ) );
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
	UNPROTECT(1); // row_names

	// set the class to data.frame
	setAttrib(out, R_ClassSymbol, mkString("data.frame"));

	// set the names
	SEXP names = getAttrib(x, R_NamesSymbol);
	SEXP names_out;
	PROTECT(names_out = allocVector( STRSXP, out_nCol ));
  
  SEXP* names_ptr = STRING_PTR(names);
  SEXP* names_out_ptr = STRING_PTR(names_out);
  for (int i=0; i < nColStack; ++i) {
    names_out_ptr[i] = names_ptr[ id_ind[i] ];
  }
	
  SET_STRING_ELT( names_out, nColStack, STRING_ELT(variable_name, 0) );
	SET_STRING_ELT( names_out, nColStack+1, STRING_ELT(value_name, 0) );
	setAttrib( out, R_NamesSymbol, names_out );
	UNPROTECT(1); // names_out

	UNPROTECT(1); // out
  return out;

}

#undef USE_RINTERNALS

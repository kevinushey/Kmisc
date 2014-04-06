#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

#define HANDLE_CASE(RTYPE, CTYPE, ACCESSSOR) \
  PROTECT(output = allocVector(RTYPE, m*n) ); \
  for (int i=0; i < m; ++i) { \
    memcpy( \
      (char*) DATAPTR(output) + i * n * sizeof(CTYPE), \
      (char*) DATAPTR( VECTOR_ELT(x, i) ), \
      n * sizeof(CTYPE) \
    ); \
  } \
  UNPROTECT(1); \
  break

// [[register]]
SEXP list2mat(SEXP x) {
  
  if (TYPEOF(x) != VECSXP) {
    error("'x' must be a list or a data.frame");
  }
  
  int m = length(x);
  int n = length( VECTOR_ELT(x, 0) );
  int type = TYPEOF( VECTOR_ELT(x, 0) );
	for (int i = 1; i < m; ++i) {
		if (length( VECTOR_ELT(x, i) ) != n) {
			error("not all columns are of equal length");
		}
    if (TYPEOF( VECTOR_ELT(x, i) ) != type) {
      error("not all columns are of the same type");
    }
	}
  SEXP output;
  switch (TYPEOF( VECTOR_ELT(x, 0) )) {
  case INTSXP: HANDLE_CASE(INTSXP, int, INTEGER);
  case REALSXP: HANDLE_CASE(REALSXP, double, REAL);
  case STRSXP: HANDLE_CASE(STRSXP, SEXP, STRING_PTR);
  case LGLSXP: HANDLE_CASE(LGLSXP, int, LOGICAL);
  default: {
    error("unimplementyed RTYPE");
    return R_NilValue;
  }
  }
  
  SEXP dims = PROTECT( allocVector(INTSXP, 2) );
  INTEGER(dims)[0] = n;
  INTEGER(dims)[1] = m;
  setAttrib(output, R_DimSymbol, dims);
  UNPROTECT(1);
  
  SEXP colnames = getAttrib(x, R_NamesSymbol);
  SEXP rownames = getAttrib(VECTOR_ELT(x, 0), R_NamesSymbol);
  
  SEXP dimnames = PROTECT( allocVector(VECSXP, 2) );
  
  if (!isNull(rownames)) {
    SET_VECTOR_ELT( dimnames, 0, rownames );
  }
  
  if (!isNull(colnames)) {
    SET_VECTOR_ELT( dimnames, 1, colnames );
  }
  
  setAttrib(output, R_DimNamesSymbol, dimnames);
  UNPROTECT(1);
  
  return output;
}

#undef HANDLE_CASE

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

inline size_t type2size(SEXP x) {
  switch (TYPEOF(x)) {
    case INTSXP: return sizeof(int);
    case REALSXP: return sizeof(double);
    case LGLSXP: return sizeof(int);
    case STRSXP: return sizeof(SEXP);
    default: error("unimplemented");
    return -1;
  }
}

#define HANDLE_CASE(RTYPE, CTYPE, ACCESSOR) \
  for (int i=0; i < ncol; ++i) { \
    SEXP tmp = PROTECT( allocVector(RTYPE, nrow) ); \
    for (int j=0; j < nrow; ++j) { \
      ACCESSOR(tmp)[j] = ACCESSOR(x)[i*nrow+j]; \
    } \
    SET_VECTOR_ELT(output, i, tmp); \
    UNPROTECT(1); \
  } \
  break

// [[export]]
SEXP mat2list(SEXP x) {
  if (!isMatrix(x)) {
    error("'x' must be a matrix");
  }
  
  SEXP dims = getAttrib(x, R_DimSymbol);
  int nrow = INTEGER(dims)[0];
  int ncol = INTEGER(dims)[1];
  SEXP output = PROTECT(allocVector(VECSXP, ncol));
  switch (TYPEOF(x)) {
  case INTSXP: HANDLE_CASE(INTSXP, int, INTEGER);
  case REALSXP: HANDLE_CASE(REALSXP, double, REAL);
  case LGLSXP: HANDLE_CASE(LGLSXP, int, LOGICAL);
  case STRSXP: HANDLE_CASE(STRSXP, SEXP, STRING_PTR);
  default: error("wtf");
  }
  UNPROTECT(1);
  
  SEXP dimnames = getAttrib(x, R_DimNamesSymbol);
  if (!isNull(dimnames)) {
    setAttrib(output, R_NamesSymbol, VECTOR_ELT(dimnames, 1));
  }
  return output;
}

#undef HANDLE_CASE

#undef USE_RINTERNALS

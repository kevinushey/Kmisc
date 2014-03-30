#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[register]]
SEXP setcolnames(SEXP x, SEXP value) {
  
  // early data.frame dispatch
  if (inherits(x, "data.frame")) {
    if (LENGTH(x) != LENGTH(value)) {
      error("'names' attributes [%i] must be the same length as the vector [%i]",
        LENGTH(value), LENGTH(x));
    }
    if (TYPEOF(value) != STRSXP)
      value = coerceVector(value, STRSXP);
    setAttrib(x, R_NamesSymbol, value);
    return x;
  }
  
  // regular matrix, array dispatch -- code is basically a 
  // direct translation of the R code for `colnames<-`, with
  // coercion checks
  int nprotect = 0;
  SEXP dn = getAttrib(x, R_DimNamesSymbol);
  SEXP dim = getAttrib(x, R_DimSymbol);
  int nd = LENGTH(dim);
  if (isNull(dn)) {
    if (isNull(value)) {
      return x;
    }
    if (nd < 2) {
      error("attempt to set 'colnames' on an object with less than two dimensions");
    }
    dn = PROTECT(allocVector(VECSXP, nd));
  }
  if (LENGTH(dn) < 2) {
    error("attempt to set 'colnames' on an object with less than two dimensions");
  }
  if (isNull(value)) {
    SET_VECTOR_ELT(dn, 1, R_NilValue);
  } else {
    if (INTEGER(dim)[1] != LENGTH(value)) {
      error("number of columns not equal to length of 'value'");
    }
    if (TYPEOF(value) != STRSXP)
      value = coerceVector(value, STRSXP);
    SET_VECTOR_ELT(dn, 1, value);
  }
  setAttrib(x, R_DimNamesSymbol, dn);
  UNPROTECT(nprotect);
  return x;
  
}

inline int df_nrows(SEXP x) {
  SEXP rownames = getAttrib(x, R_RowNamesSymbol);
  if (isNull(rownames)) error("'row.names' attribute is NULL");
  if (TYPEOF(rownames) == INTSXP && LENGTH(rownames) == 2 && INTEGER(rownames)[0] == NA_INTEGER) {
    return INTEGER(rownames)[1];
  } else {
    return LENGTH(rownames);
  }
}

// [[register]]
SEXP setrownames(SEXP x, SEXP value) {
  
  // early data.frame dispatch
  if (inherits(x, "data.frame")) {
    
    if (isNull(value)) {
      SEXP rownames = allocVector(INTSXP, 2);
      INTEGER(rownames)[0] = NA_INTEGER;
      INTEGER(rownames)[1] = df_nrows(x);
      setAttrib(x, R_RowNamesSymbol, rownames);
      return x;
    }
    
    if (LENGTH(value) != df_nrows(x)) {
      error("row names should be the same length as nrow(x)");
    }
    
    if (TYPEOF(value) != STRSXP) {
      value = coerceVector(value, STRSXP);
    }
    setAttrib(x, R_RowNamesSymbol, value);
    return x;
  }
  
  // regular matrix dispatch
  int nprotect = 0;
  SEXP dn = getAttrib(x, R_DimNamesSymbol);
  SEXP dim = getAttrib(x, R_DimSymbol);
  int nd = LENGTH(dim);
  if (isNull(dn)) {
    if (isNull(value)) {
      return x;
    }
    if (nd < 1) {
      error("attempt to set 'rownames' on an object with no dimensions");
    }
    dn = PROTECT(allocVector(VECSXP, nd));
  }
  if (LENGTH(dn) < 1) {
    error("attempt to set 'rownames' on an object with no dimensions");
  }
  if (isNull(value)) {
    SET_VECTOR_ELT(dn, 0, R_NilValue);
  } else {
    if (INTEGER(dim)[0] != LENGTH(value)) {
      error("number of rows not equal to length of 'value'");
    }
    SET_VECTOR_ELT(dn, 0, value);
  }
  setAttrib(x, R_DimNamesSymbol, dn);
  UNPROTECT(nprotect);
  return x;
  
}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

char max_type1(SEXP x) {
  if (TYPEOF(x) != VECSXP) {
    error("Expected a VECSXP but got a '%s'", type2char(TYPEOF(x)));
  }
  int n = length(x);
  char max_type = -1;
  char tmp = -1;
  for (int i=0; i < n; ++i) {
    // factors should mean we coerce to string
    if (isFactor(VECTOR_ELT(x, i))) {
      if (STRSXP > max_type) {
        max_type = STRSXP;
      }
    } else if ((tmp = TYPEOF( VECTOR_ELT(x, i) )) > max_type) {
      max_type = tmp;
    }
  }
  return max_type;
}

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

void set_names(SEXP x) {
  int m = length(x);
  SEXP nm = PROTECT( allocVector(STRSXP, m) );
  char str[ (int) log10(m) + 3];
  for (int i = 0; i < m; ++i) {
  	sprintf(str, "%s%i", "V", i + 1);
		SET_STRING_ELT(nm, i, mkChar(str));
	}
	setAttrib(x, R_NamesSymbol, nm);
  UNPROTECT(1);
}

void set_rownames(SEXP x) {
  int n = length(VECTOR_ELT(x, 0));
  SEXP rownames = PROTECT( allocVector(INTSXP, 2) );
  INTEGER(rownames)[0] = NA_INTEGER;
  INTEGER(rownames)[1] = -n;
  setAttrib(x, R_RowNamesSymbol, rownames);
  UNPROTECT(1);
}

#undef USE_RINTERNALS

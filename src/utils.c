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

#undef USE_RINTERNALS

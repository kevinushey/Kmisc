#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

// utils.c
char max_type1(SEXP);

// [[export]]
SEXP transpose_list(SEXP x_) {
  
  SEXP x;
  bool do_coerce = false;
  int n = length(x_);
  int N = length(VECTOR_ELT(x_, 0));
  char type = max_type1(x_);
  
  for (int i=0; i < n; ++i) {
    
    if (length(VECTOR_ELT(x_, i)) != N) {
      Rf_error("Each element in the list must be of the same length");
    }
    
  }
    
  for (int i=0; i < n; ++i) {    
    if (TYPEOF( VECTOR_ELT(x_, i) ) != type) {
      Rf_warning("Coercing vectors in the list to type '%s'", type2char(type));
      do_coerce = true;
      break;
    }
  }
  
  if (do_coerce) {
    x = PROTECT( duplicate(x_) );
    for (int i=0; i < n; ++i) {
      if (TYPEOF(VECTOR_ELT(x, i)) != type) {
        SET_VECTOR_ELT(x, i, coerceVector(VECTOR_ELT(x, i), type));
      }
    }
  } else {
    x = x_;
  }
  
  SEXP output = PROTECT( allocVector(VECSXP, N) );
  
  #define HANDLE_CASE(RTYPE, CTYPE, ACCESSOR) { \
    for (int j=0; j < N; ++j) { \
      SET_VECTOR_ELT(output, j, allocVector(RTYPE, n)); \
      CTYPE* ptr = ACCESSOR( VECTOR_ELT(output, j) ); \
      for (int i=0; i < n; ++i) { \
        ptr[i] = ACCESSOR( VECTOR_ELT(x, i) )[j]; \
      } \
    } \
    break; \
  } \
    
  switch (type) {
  case LGLSXP: HANDLE_CASE(LGLSXP, int, LOGICAL);
  case INTSXP: HANDLE_CASE(INTSXP, int, INTEGER);
  case REALSXP: HANDLE_CASE(REALSXP, double, REAL);
  case STRSXP: HANDLE_CASE(STRSXP, SEXP, STRING_PTR);
  }
  
  #undef HANDLE_CASE
  
  UNPROTECT(1);
  if (do_coerce) UNPROTECT(1);
  return output;
  
}

#undef USE_RINTERNALS

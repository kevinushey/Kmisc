#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

SEXP recurse_factor_to_char( SEXP X, SEXP parent, int i ) {
  
  if( TYPEOF(X) == VECSXP ) {
    for( int j=0; j < length(X); ++j ) {
      recurse_factor_to_char( VECTOR_ELT(X, j), X, j );
    }
  } else {
    if( isFactor(X) ) {
      SET_VECTOR_ELT( parent, i, asCharacterFactor(X) );
    }
  }
  return X;
  
}

SEXP factor_to_char( SEXP X_ ) {
  SEXP X;
  PROTECT( X = duplicate(X_) );
  if( TYPEOF(X) == VECSXP ) {
    SEXP out = recurse_factor_to_char( X, X, 0);
    UNPROTECT(1);
    return out;
  } else {
    if( isFactor(X) ) {
      SEXP out = asCharacterFactor(X);
      UNPROTECT(1);
      return out;
    } else {
      warning("X is neither a list nor a factor; no change done");
      UNPROTECT(1);
      return X;
    }
  }
}

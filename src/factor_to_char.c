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

SEXP factor_to_char( SEXP X_, SEXP inplace_ ) {
  
  int inplace = asInteger(inplace_);
  int numprotect = 0;
  SEXP X;
  if (inplace) {
    X = X_;
  } else {
    PROTECT( X = duplicate(X_) );
    ++numprotect;
  }
  if( TYPEOF(X) == VECSXP ) {
    SEXP out = recurse_factor_to_char( X, X, 0);
    UNPROTECT(numprotect);
    return out;
  } else {
    if( isFactor(X) ) {
      SEXP out = asCharacterFactor(X);
      UNPROTECT(numprotect);
      return out;
    } else {
      warning("X is neither a list nor a factor; no change done");
      UNPROTECT(numprotect);
      return X;
    }
  }
}

#undef USE_RINTERNALS

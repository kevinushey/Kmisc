#include <R.h>
#include <Rinternals.h>

#define USE_RINTERNALS

SEXP in_interval( SEXP x, SEXP lo, SEXP hi, 
        SEXP include_lower, SEXP include_upper ) {
  
  int len = Rf_length(x);
  double lower = REAL(lo)[0], upper = REAL(hi)[0], *xp = REAL(x);
  
  int inc_lower = asLogical(include_lower);
  int inc_upper = asLogical(include_upper);
  
  SEXP out = PROTECT( allocVector( LGLSXP, len ) );
  int *outp = LOGICAL(out);
  
  if( inc_lower == 1 && inc_upper == 1 ) {
      for( int i=0; i < len; ++i ) {
        outp[i] = xp[i] >= lower && xp[i] <= upper;
        }
  }
  
  if( inc_lower == 1 && inc_upper == 0 ) {
      for( int i=0; i < len; ++i ) {
        outp[i] = xp[i] >= lower && xp[i] < upper;
        }
  }
  
  if( inc_lower == 0 && inc_upper == 1 ) {
      for( int i=0; i < len; ++i ) {
        outp[i] = xp[i] > lower && xp[i] <= upper;
        }
  }
  
  if( inc_lower == 0 && inc_upper == 0 ) {
      for( int i=0; i < len; ++i ) {
        outp[i] = xp[i] > lower && xp[i] < upper;
        }
  }
  
  UNPROTECT(1);
  return out;
  
}

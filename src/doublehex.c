#include <R.h>
#include <Rinternals.h>

// [[export]]
SEXP double2hex(SEXP d_) {
  
  if (TYPEOF(d_) != REALSXP || LENGTH(d_) != 1) {
    error("'x' must be a numeric vector of length 1");
  }
  
  double d = REAL(d_)[0];
  
  unsigned char *buffer = (unsigned char*)&d;
  int bufferSize = sizeof(double);
  
  char converted[bufferSize * 2 + 1];
  
  int j = 0;
  for(int i = 0 ; i < bufferSize ; ++i)
  {
    sprintf(&converted[j*2], "%02X", buffer[i]);
    ++j;
  }
  
  SEXP output = PROTECT( allocVector(STRSXP, 1) );
  SET_STRING_ELT(output, 0, mkChar(converted));
  return output;
}

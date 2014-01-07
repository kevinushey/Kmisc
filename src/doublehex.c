#include <R.h>
#include <Rinternals.h>

// [[export]]
SEXP double2hex(SEXP x) {
  
  if (sizeof(unsigned long long) != sizeof(double)) {
    error("this code assumes sizeof(unsigned long long) and sizeof(double) are identical");
  }
  
  // double is 8 bytes, each byte can be represented by 2 hex chars,
  // so need a str with 16+1 slots
  int n = sizeof(unsigned long long) * 2 + 1;
  
  unsigned long long *xx = (unsigned long long*) REAL(x);
  char buf[n];
  for (int i=0; i < n-1; ++i) {
    buf[i] = '0';
  }
  snprintf(buf, n, "%llX", *xx);
  SEXP output = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(output, 0, mkChar(buf));
  UNPROTECT(1);
  return output;
}

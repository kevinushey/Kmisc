#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

#if defined(__GNUC__) || defined(__clang__)
#define STATIC_ASSERT(x) \
  do { \
    const static char __attribute__((__unused__)) dummy[x ? 1 : -1]; \
  } while (0)
#else
#define STATIC_ASSERT(x) \
  do { \
    const static char dummy[x ? 1 : -1]; \
  } while (0)
#endif

// [[export]]
SEXP double2hex(SEXP x) {
  
  STATIC_ASSERT( sizeof(unsigned long long) == sizeof(double) );
  
  // double is 8 bytes, each byte can be represented by 2 hex chars,
  // so need a str with 16+1 slots
  int n = sizeof(unsigned long long) * 2 + 1;
  
  unsigned long long *xx = (unsigned long long*) REAL(x);
  char buf[n];
  snprintf(buf, n, "%016llX", *xx);
  SEXP output = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(output, 0, mkChar(buf));
  UNPROTECT(1);
  return output;
}

// [[export]]
SEXP int2hex(SEXP x) {
  
  int n = sizeof(int) * 2 + 1;
  char buf[n];
  snprintf(buf, n, "%08X", *(unsigned int*)INTEGER(x));
  SEXP output = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(output, 0, mkChar(buf));
  UNPROTECT(1);
  return output;
}

#undef USE_RINTERNALS

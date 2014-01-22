// This code is mainly a copied + cleaned up version of do_lapply in the
// R sources (see src/main/apply.c). The main difference is that the
// function call is constructed through passing the index as the second
// argument.

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[export]]
SEXP enumerate(SEXP call, SEXP env, SEXP nargs_) {

  int nargs = INTEGER(nargs_)[0];

  SEXP args = CDR(call);
  // non-standard evaluation can hide the symbol pointing
  // at the object we want; we rely on enumerate having 'X' pointing
  // where we need it to
  SEXP vecSym = install("X"); args = CDR(args);
  SEXP funSym = CAR(args); args = CDR(args);

  SEXP XX = PROTECT( eval(vecSym, env) );
  R_xlen_t n = xlength(XX);

  Rboolean realIndex = n > INT_MAX;

  SEXP ans = PROTECT(allocVector(VECSXP, n));
  SEXP names = getAttrib(XX, R_NamesSymbol);
  if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);

  /* Build call: FUN(XX[[<ind>]], <ind>, ...) */

  /* Notice that it is OK to have one arg to LCONS do memory
     allocation and not PROTECT the result (LCONS does memory
     protection of its args internally), but not both of them,
     since the computation of one may destroy the other */

  SEXP ind = PROTECT(allocVector(realIndex ? REALSXP : INTSXP, 1));
  SEXP tmp;
  /* The R level code has ensured that XX is a vector.
     If it is atomic we can speed things up slightly by
     using the evaluated version.
  */
  if (isVectorAtomic(XX)) {
    tmp = PROTECT(tmp = LCONS(R_Bracket2Symbol,
  		  LCONS(XX, LCONS(ind, R_NilValue))));
  } else {
    tmp = PROTECT(LCONS(R_Bracket2Symbol,
  	    LCONS(vecSym, LCONS(ind, R_NilValue))));
  }

  SEXP R_fcall;
  if (nargs < 2) {
    R_fcall = PROTECT(LCONS(funSym,
  		 LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));
  } else if (nargs == 2) {
    R_fcall = PROTECT(LCONS(funSym,
  		 LCONS(tmp, LCONS(ind, LCONS(R_DotsSymbol, R_NilValue)))));
  } else {
    error("'enumerate' can't handle functions with more than 2 non-dot args");
  }

  for (R_xlen_t i = 0; i < n; i++) {
    if (realIndex) REAL(ind)[0] = (double)(i + 1);
    else INTEGER(ind)[0] = (int)(i + 1);
    tmp = eval(R_fcall, env);
    if (NAMED(tmp)) tmp = duplicate(tmp);
    SET_VECTOR_ELT(ans, i, tmp);
  }

  UNPROTECT(5);
  return ans;
}

#undef USE_RINTERNALS

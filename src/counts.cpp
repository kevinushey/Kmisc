#include <Rcpp.h>
#include <Rdefines.h>
using namespace Rcpp;

template <int RTYPE>
IntegerVector do_counts( const Vector<RTYPE>& x ) {
  IntegerVector output = table(x);
  if (Rf_isFactor(x)) {
    Rf_setAttrib(output, R_NamesSymbol, Rf_getAttrib(x, R_LevelsSymbol));
  }
  // fix names
  CharacterVector names = output.attr("names");
  for (int i=0; i < output.size(); ++i) {
    if (names[i] == "-0") {
      names[i] = "0";
    }
  }
  output.attr("names") = names;
  return output;
}

// [[Rcpp::export]]
SEXP counts( SEXP x ) {
  switch( TYPEOF(x) ) {
  case INTSXP: return do_counts<INTSXP>(x);
  case REALSXP: return do_counts<REALSXP>(x);
  case STRSXP: return do_counts<STRSXP>(x);
  case LGLSXP: return do_counts<LGLSXP>(x);
  default: {
    Rf_error("'x' is of invalid type '%s'", Rf_type2char( TYPEOF(x) ));
    return R_NilValue;
  }
  }
}

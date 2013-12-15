#include <Rcpp.h>
#include <Rdefines.h>
using namespace Rcpp;

// we handle the integer, numeric, character cases all separately
IntegerVector do_counts_integer( const IntegerVector& x ) {
  
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

IntegerVector do_counts_numeric( const NumericVector& x ) {
  std::map<double, int> counts;
  int numNA = 0;
  for (int i=0; i < x.size(); ++i) {
    if (ISNA(x[i])) {
      ++numNA;
    } else {
      ++counts[ x[i] ];
    }
  }
  
  IntegerVector output = wrap(counts);
  
  // fix names
  CharacterVector names = output.attr("names");
  for (int i=0; i < output.size(); ++i) {
    if (names[i] == "-0") {
      names[i] = "0";
    }
  }
  output.attr("names") = names;
  
  // append NA
  if (numNA > 0) {
    output.push_back(numNA);
    CharacterVector names = output.attr("names");
    names[ names.size()-1 ] = NA_STRING;
    output.attr("names") = names;
  }
  
  return output;
}

IntegerVector do_counts_character( const CharacterVector& x ) {
  IntegerVector output = table(x);
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

inline IntegerVector do_counts_logical( const LogicalVector& x ) {
  return table(x);
}

// [[Rcpp::export]]
SEXP counts( SEXP x ) {
  switch( TYPEOF(x) ) {
  case INTSXP: return do_counts_integer(x);
  case REALSXP: return do_counts_numeric(x);
  case STRSXP: return do_counts_character(x);
  case LGLSXP: return do_counts_logical(x);
  default: {
    Rf_error("'x' is of invalid type '%s'", Rf_type2char( TYPEOF(x) ));
    return R_NilValue;
  }
  }
}

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP fast_factor(SEXP x, SEXP levels) {
  Function factor("factor_");
  return factor(x, levels);
}

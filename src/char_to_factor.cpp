#include <Rcpp.h>
using namespace Rcpp;

SEXP fast_factor(SEXP, SEXP);

// [[Rcpp::export(.char_to_factor)]]
RObject char_to_factor( RObject x_, bool inplace ) {
   
  RObject x;
  if (inplace) {
    x = x_;
  } else {
    x = clone(x_);
  }
  
  if (TYPEOF(x) == VECSXP) {
    int n = Rf_length(x);
    for (int i=0; i < n; ++i) {
      SET_VECTOR_ELT(x, i, char_to_factor( VECTOR_ELT(x, i), false ));
    }
  } else if (TYPEOF(x) == STRSXP) {
    x = fast_factor(x, sort_unique( as<CharacterVector>(x) ));
  }
  
  return x;
  
}

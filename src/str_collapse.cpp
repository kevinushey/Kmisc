#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector str_collapse(List x) {
  int n = x.size();
  CharacterVector output = no_init(n);
  for (int i=0; i < n; ++i) {
    output[i] = collapse( as<CharacterVector>(x[i]) );
  }
  output.attr("names") = as<CharacterVector>(x.attr("names"));
  return output;
}

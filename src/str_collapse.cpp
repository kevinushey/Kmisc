#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector str_collapse_list(List x) {
  int n = x.size();
  CharacterVector output = no_init(n);
  for (int i=0; i < n; ++i) {
    output[i] = collapse( as<CharacterVector>(x[i]) );
  }
  
  output.attr("names") = x.attr("names");
  
  return output;
}

// [[Rcpp::export]]
String str_collapse_str(CharacterVector x) {
  return collapse(x);
}

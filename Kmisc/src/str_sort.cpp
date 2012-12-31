#include <R.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

RcppExport SEXP str_sort( SEXP x, SEXP increasing ) {
  
  // Transfer the R objects to a C++ representation as a vector of strings
  CharacterVector xx(x);
  bool inc = as<bool>(increasing);
  
  vector<string> strings = as< vector<string> >(xx);
  
  if( inc ) {
    
    for( int i=0; i < xx.size(); i++ ) {
      sort( strings[i].begin(), strings[i].end() );
      }
      
  } else {
    
    for( int i=0; i < xx.size(); i++ ) {
        sort( strings[i].begin(), strings[i].end() );
        reverse( strings[i].begin(), strings[i].end() );
      }
    
  }
    
  // return the strings
  return wrap(strings);
  
}
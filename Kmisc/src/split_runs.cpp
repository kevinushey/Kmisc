#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List split_runs_numeric( NumericVector X ) {
  
  List out( X.size() );
  
  std::vector< std::vector< double > > all_nums;
  std::vector< double > curr_nums;
  
  // initial stuff
  curr_nums.push_back( X[0] );
  
  for( NumericVector::iterator it = X.begin()+1; it != X.end(); ++it ) {
    if( (*it) != (*(it-1)) ) {
      all_nums.push_back( curr_nums );
      curr_nums.clear();
      curr_nums.push_back( *it );
    } else {
      curr_nums.push_back( *it );
    }
  }
  
  // push the final vector in
  all_nums.push_back( curr_nums );
  
  return wrap( all_nums );
  
}

// [[Rcpp::export]]
List split_runs_character( std::vector< std::string > X ) {
  
  std::vector< std::vector< std::string > > all_nums;
  std::vector< std::string > curr_nums;
  
  // initial stuff
  curr_nums.push_back( X[0] );
  
  for( std::vector< std::string >::iterator it = X.begin() + 1; it != X.end(); ++it ) {
    if( *it != *(it-1) ) {
      all_nums.push_back( curr_nums );
      curr_nums.clear();
      curr_nums.push_back( *it );
    } else {
      curr_nums.push_back( *it );
    }
  }
  
  // push the final vector in
  all_nums.push_back( curr_nums );
  
  return wrap( all_nums );
  
}

// [[Rcpp::export]]
List split_runs_one( std::string x ) {
  
  std::vector< std::string > out;
  
  std::string curr_str;
  curr_str.append( x, 0, 1 );
  
  for( std::string::const_iterator it = x.begin()+1; it != x.end(); ++it ) {
    if( *it != *(it-1) ) {
      out.push_back( curr_str );
      curr_str.erase();
      curr_str.push_back( *it );
    } else {
      curr_str.push_back( *it );
    }
  }
  
  out.push_back( curr_str );
  
  return wrap(out);
  
}

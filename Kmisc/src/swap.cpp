#include <Rcpp.h>
using namespace Rcpp;

inline void check_type( SEXP x, std::string name ) {
  switch( TYPEOF(x) ) {
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case LGLSXP:
      break;
    default:
      Rf_error("Argument '%s' is of incompatible type '%s'", 
        name.c_str(), 
        Rf_type2char( TYPEOF(x) )
      );
  }
}

template <int RTYPE>
Vector<RTYPE> do_swap( const Vector<RTYPE> vec, const Vector<RTYPE>& from, const Vector<RTYPE>& to ) {
  IntegerVector matches = match(vec, from) - 1;
  int n = vec.size();
  Vector<RTYPE> out = no_init(n);
  for( int i=0; i < n; ++i ) {
    // Rcout << "matches[" << i << "] is: " << matches[i] << std::endl;
    if( !IntegerVector::is_na( matches[i] ) ) {
      // Rcout << "\tmatches[" << i << "] is not NA" << std::endl;
      out[i] = to[ matches[i] ];
    } else {
      // Rcout << "\tmatches[" << i << "] is NA" << std::endl;
      out[i] = vec[i];
    }
  }
  return out;
}

// [[Rcpp::export]]
SEXP swap( SEXP vec, SEXP from, SEXP to ) {
  
  check_type(vec, "vec");
  check_type(from, "from");
  check_type(to, "to");
  
  if( TYPEOF(to) > TYPEOF(from) ) {
    from = Rf_coerceVector(from, TYPEOF(to));
  } else if( TYPEOF(from) > TYPEOF(to) ) {
    to = Rf_coerceVector(to, TYPEOF(from));
  }
  
  switch( TYPEOF(to) ) {
    case INTSXP: return do_swap<INTSXP>(vec, from, to);
    case REALSXP: return do_swap<REALSXP>(vec, from, to);
    case STRSXP: return do_swap<STRSXP>(vec, from, to);
    case LGLSXP: stop("Can't swap from logical to logical");
    default: stop("incompatible RTYPE");
  }
  
  return R_NilValue;
  
}

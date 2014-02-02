#include <Rcpp.h>
#include <utils.hpp>
using namespace Rcpp;

namespace Kmisc {
  
  template <typename T>
  bool any_na(T x) {
    return false;
  }
  
  template <>
  bool any_na<IntegerVector>(IntegerVector x) {
    int n = x.size();
    for (int i=0; i < n; ++i) {
      if (x[i] == NA_INTEGER) return true;
    }
    return false;
  }
  
  template <>
  bool any_na<CharacterVector>(CharacterVector x) {
    int n = x.size();
    for (int i=0; i < n; ++i) {
      if (x[i] == NA_STRING) return true;
    }
    return false;
  }
  
  template <>
  bool any_na<NumericVector>(NumericVector x) {
    int n = x.size();
    for (int i=0; i < n; ++i) {
      if (IsNA(x[i])) return true;
    }
    return false;
  }
  
  template <int RTYPE, template <class> class StoragePolicy>
  Vector<RTYPE, StoragePolicy> sort_unique(const Vector<RTYPE, StoragePolicy>& x) {
    typedef typename ::Rcpp::traits::storage_type<RTYPE>::type STORAGE;
    typedef typename std::set< STORAGE, NACompare<STORAGE> > SET;
    typedef typename std::set< STORAGE, NACompare<STORAGE> >::iterator ITERATOR;
    SET set( x.begin(), x.end() );
    int n = set.size();
    Vector<RTYPE, StoragePolicy> output = no_init(n);
    ITERATOR itr = set.begin();
    for (int i=0; i < n; ++i, ++itr) {
      output[i] = *itr;
    }
    return output;
  }
  
}

template <int RTYPE>
IntegerVector fast_factor_template( const Vector<RTYPE>& x, SEXP levels, bool isNull ) {

  Vector<RTYPE> sorted;
  if (isNull) {
    sorted = Kmisc::sort_unique(x);
  } else {
    sorted = Vector<RTYPE>(levels);
  }
  
	IntegerVector out = match(x, sorted);

	if (isNull) {
    // handle NAs
    if (any(is_na(sorted))) {
      
      // by default, NAs are placed at the end
      CharacterVector levels( sorted.begin(), sorted.end()-1 );
      
      // values greater than the length of levels should be NA-ed
      int n = LENGTH(levels);
    	for (IntegerVector::iterator it = out.begin(); it != out.end(); ++it) {
  			if (*it > n) {
  				*it = NA_INTEGER;
  			}
  		}
      
      out.attr("levels") = as<CharacterVector>( sorted );
  
  	} else {
  		out.attr("levels") = as<CharacterVector>( sorted );
  	}
  } else {
    out.attr("levels") = as<CharacterVector>( levels );
  }
  
  out.attr("class") = "factor";
  return out;

}

template <>
IntegerVector fast_factor_template<REALSXP>( const Vector<REALSXP>& x, SEXP levels, bool isNull ) {

  Vector<REALSXP> sorted;
  if (isNull) {
    sorted = Kmisc::sort_unique(x);
  } else {
    sorted = Vector<REALSXP>(levels);
  }
  
  IntegerVector out = match(x, sorted);
  
  // make sure NaNs propagate correctly
  int maxplus1 = std::max(0, *std::max_element(out.begin(), out.end())) + 1;
  for (int i=0; i < out.size(); ++i) {
    if (IsNaN(x[i])) {
      out[i] = maxplus1;
    }
  }

	if (isNull) {
    
    if (Kmisc::any_na(sorted)) {
      
      // by default, NAs are placed at the end
      CharacterVector levels( sorted.begin(), sorted.end()-1 );
      
      // values greater than the length of levels should be NA-ed
      int n = LENGTH(levels);
      for (IntegerVector::iterator it = out.begin(); it != out.end(); ++it) {
  			if (*it > n) {
  				*it = NA_INTEGER;
  			}
  		}
      
      out.attr("levels") = as<CharacterVector>( sorted );
  
  	} else {
  		out.attr("levels") = as<CharacterVector>( sorted );
  	}
  } else {
    out.attr("levels") = as<CharacterVector>( levels );
  }
  
  out.attr("class") = "factor";
  return out;

}

// [[Rcpp::export]]
SEXP fast_factor( SEXP x, SEXP levels ) {
  int type = TYPEOF(x);
  if (!Rf_isNull(levels) && TYPEOF(levels) != type) {
    levels = Rf_coerceVector(levels, type);
  }
  bool isNull = Rf_isNull(levels);
	switch (type) {
	case INTSXP: return fast_factor_template<INTSXP>(x, levels, isNull);
	case REALSXP: return fast_factor_template<REALSXP>(x, levels, isNull);
	case STRSXP: return fast_factor_template<STRSXP>(x, levels, isNull);
	case LGLSXP: return fast_factor_template<INTSXP>(x, levels, isNull);
	}
	Rf_error("argument is of incompatible type '%s'", Rf_type2char( TYPEOF(x) ));
	return R_NilValue;
}

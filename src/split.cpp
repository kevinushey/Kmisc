#include <Rcpp.h>
#include "utils.h"

using namespace Rcpp;

template <int T1, int T2>
SEXP do_split(const Vector<T1>& x, const Vector<T2>& y, int na_last) {
  
  int nx = LENGTH(x);
  
  typedef typename traits::storage_type<T1>::type VectorStorage;
  typedef typename traits::storage_type<T2>::type SplitStorage;
  
  std::map< 
    SplitStorage, 
    std::vector< VectorStorage >,
    NACompare<SplitStorage>
  > output;
  
  if (na_last) {
    for (int i=0; i < nx; ++i) {
      output[ y[i] ].push_back( x[i] );
    }
  } else {
    for (int i=0; i < nx; ++i) {
      if (!IsNA(y[i]))
        output[ y[i] ].push_back( x[i] );
    }
  }
  
  return wrap(output);
}

template <int T2>
SEXP do_split_lgcl(const LogicalVector& x, const Vector<T2>& y, int na_last) {
  
  int nx = LENGTH(x);
  typedef typename traits::storage_type<T2>::type SplitStorage;
  
  std::map< 
    SplitStorage, 
    std::vector<int>,
    NACompare<SplitStorage>
  > output;
  
  if (na_last) {
    for (int i=0; i < nx; ++i) {
      output[ y[i] ].push_back( x[i] );
    }
  } else {
    for (int i=0; i < nx; ++i) {
      if (!IsNA(y[i]))
        output[ y[i] ].push_back( x[i] );
    }
  }
  
  SEXP output_ = wrap(output);
  int no = LENGTH(output_);
  for (int i=0; i < no; ++i) {
    SET_VECTOR_ELT(output_, i, Rf_coerceVector( VECTOR_ELT(output_, i), LGLSXP ));
  }
  return output_;
}

#define DISPATCH(XTYPE) case XTYPE: { \
  switch (TYPEOF(y)) { \
    case INTSXP: return do_split( as< Vector<XTYPE> >(x), as< Vector<INTSXP> >(y), na_last ); \
    case REALSXP: return do_split( as< Vector<XTYPE> >(x), as< Vector<REALSXP> >(y), na_last ); \
    case LGLSXP: return do_split( as< Vector<XTYPE> >(x), as< Vector<LGLSXP> >(y), na_last ); \
    case STRSXP: return do_split( as< Vector<XTYPE> >(x), as< Vector<STRSXP> >(y), na_last ); \
  } \
}

#define DISPATCH_CHAR(X) case STRSXP: { \
  switch (TYPEOF(y)) { \
    case INTSXP: return do_split_char( as< Vector<STRSXP> >(x), as< Vector<INTSXP> >(y), na_last ); \
    case REALSXP: return do_split_char( as< Vector<STRSXP> >(x), as< Vector<REALSXP> >(y), na_last ); \
    case LGLSXP: return do_split_char( as< Vector<STRSXP> >(x), as< Vector<LGLSXP> >(y), na_last ); \
    case STRSXP: return do_split_char( as< Vector<STRSXP> >(x), as< Vector<STRSXP> >(y), na_last ); \
  } \
}

#define DISPATCH_LGCL(X) case LGLSXP: { \
  switch (TYPEOF(y)) { \
    case INTSXP: return do_split_lgcl( as< Vector<LGLSXP> >(x), as< Vector<INTSXP> >(y), na_last ); \
    case REALSXP: return do_split_lgcl( as< Vector<LGLSXP> >(x), as< Vector<REALSXP> >(y), na_last ); \
    case LGLSXP: return do_split_lgcl( as< Vector<LGLSXP> >(x), as< Vector<LGLSXP> >(y), na_last ); \
    case STRSXP: return do_split_lgcl( as< Vector<LGLSXP> >(x), as< Vector<STRSXP> >(y), na_last ); \
  } \
}

// [[Rcpp::export(.split)]]
SEXP split(SEXP x, SEXP y, SEXP na_last_) {
  
  int na_last = INTEGER(na_last_)[0];
  
  if (LENGTH(x) != LENGTH(y)) {
    stop("'x' and 'y' must have identical lengths");
  }
  
  switch (TYPEOF(x)) {
    DISPATCH(INTSXP);
    DISPATCH_LGCL(LGLSXP);
    DISPATCH(REALSXP);
    default: {
      stop("Unhandled RTYPE");
      return R_NilValue;
    }
  }
}

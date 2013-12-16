#include <Rcpp.h>
using namespace Rcpp;

template <typename T>
class NACompare;

template <>
class NACompare<int> {
  public:
    inline bool operator()(int left, int right) const {
      if (left == NA_INTEGER) return false;
      if (right == NA_INTEGER) return true;
      return left < right;
    }
};

template <>
class NACompare<double> {
  public:
    inline bool operator()(double left, double right) const {
      bool leftNaN = (left != left);
      bool rightNaN = (right != right);
      if (leftNaN != rightNaN) {
        return leftNaN < rightNaN;
      } else {
        return left < right;
      }
    }
};

template <>
class NACompare<String> {
  public:
    inline bool operator()(const String& left, const String& right) const {
      return left < right;
    }
};

template <typename T, typename U>
inline IntegerVector do_counts(const T& x) {
  std::map< U, int, NACompare<U> > counts;
  int n = x.size();
  for (int i=0; i < n; ++i) {
    ++counts[ x[i] ];
  }
  IntegerVector output = wrap(counts);
  return wrap(counts);
}

// [[Rcpp::export]]
IntegerVector counts(SEXP x) {
  switch (TYPEOF(x)) {
  case REALSXP: {
    IntegerVector output = do_counts<NumericVector, double>(x);
    // fix names
    CharacterVector names = output.attr("names");
    CharacterVector::iterator it = std::find( names.begin(), names.end(), "-0" );
    if (it != names.end()) {
      *it = "0";
    }
    output.attr("names") = names;
    return output;
  }
  case STRSXP: return do_counts<CharacterVector, String>(x);
  case INTSXP: return do_counts<IntegerVector, int>(x);
  case LGLSXP: {
    IntegerVector output = do_counts<LogicalVector, int>(x);
    SET_STRING_ELT( output.attr("names"), 0, Rf_mkChar("FALSE") );
    SET_STRING_ELT( output.attr("names"), 1, Rf_mkChar("TRUE") );
    return output;
  }
  default: {
    stop("unrecognized SEXP type");
    return R_NilValue;
  }
  }
}

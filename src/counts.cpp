#include <Rcpp.h>
using namespace Rcpp;

template <typename T>
struct NACompare;

template <>
struct NACompare<int> {
  inline bool operator()(int left, int right) const {
    if (left == NA_INTEGER) return false;
    if (right == NA_INTEGER) return true;
    return left < right;
  }
};

template <>
struct NACompare<double> {  
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
struct NACompare<const char*> {
  inline bool operator()(const char* left, const char* right) const {
    return strcmp(left, right) < 0;
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

template <>
inline IntegerVector do_counts<CharacterVector, const char*>(const CharacterVector& x) {
  IntegerVector tmp = table(x);
  std::map< const char*, int, NACompare<const char*> > counts;
  for (int i=0; i < tmp.size(); ++i) {
    counts[ CHAR(STRING_ELT( tmp.attr("names"), i )) ] = tmp[i];
  }
  IntegerVector output = wrap(counts);
  
  CharacterVector names = output.attr("names");
  CharacterVector::iterator it = std::find( names.begin(), names.end(), "NA" );
  if (it != names.end()) {
    *it = NA_STRING;
  }
  return output;
  
}

template <>
inline IntegerVector do_counts<NumericVector, double>(const NumericVector& x) {
  std::map< double, int, NACompare<double> > counts;
  int n = x.size();
  for (int i=0; i < n; ++i) {
    ++counts[ x[i] ];
  }
  IntegerVector output = wrap(counts);
  
  // explicitly use R's double-to-character coercion to get good names
  int m = counts.size();
  NumericVector keys = no_init(m);
  typedef std::map< double, int, NACompare<double> >::iterator MapItr;
  int i = 0;
  for (MapItr it = counts.begin(); it != counts.end(); ++it) {
    keys[i] = it->first;
    ++i;
  }
  CharacterVector names = Rf_coerceVector(keys, STRSXP);
  output.attr("names") = names;
  return output;
}

// [[Rcpp::export]]
IntegerVector tableRcpp(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP: return table(as<IntegerVector>(x));
  case REALSXP: return table(as<NumericVector>(x));
  case STRSXP: return table(as<CharacterVector>(x));
  case LGLSXP: return table(as<LogicalVector>(x));
  default: {
    stop("unrecognized SEXP type");
    return R_NilValue;
  }
  }
}

// [[Rcpp::export]]
IntegerVector counts(SEXP x) {
  switch (TYPEOF(x)) {
  case REALSXP: {
    IntegerVector output = do_counts<NumericVector, double>(x);
    // fix names
    for (int i=0; i < output.size(); ++i) {
      if (CHAR(STRING_ELT(output.attr("names"), i)) == "-0") {
        SET_STRING_ELT(output.attr("names"), i, Rf_mkChar("-0"));
      }
    }
    return output;
  }
  case STRSXP: return do_counts<CharacterVector, const char*>(x);
  case INTSXP: return do_counts<IntegerVector, int>(x);
  case LGLSXP: {
    IntegerVector output = do_counts<LogicalVector, int>(x);
    // yuck
    SEXP namesptr = Rf_getAttrib(output, R_NamesSymbol);
    for (int i=0; i < output.size(); ++i) {
      if (strcmp(CHAR(STRING_ELT(namesptr, i)), "0") == 0) {
        SET_STRING_ELT(namesptr, i, Rf_mkChar("FALSE"));
      }
      if (strcmp(CHAR(STRING_ELT(namesptr, i)), "1") == 0) {
        SET_STRING_ELT(namesptr, i, Rf_mkChar("TRUE"));
      }
    }
    return output;
  }
  default: {
    stop("unrecognized SEXP type");
    return R_NilValue;
  }
  }
}

/*** R
set.seed(123)
x <- round( rnorm(1E6), 1 )
x[sample(length(x), 100)] <- NA
#x[sample(length(x), 100)] <- NaN
x[sample(length(x), 100)] <- Inf
x[sample(length(x), 100)] <- -Inf
x_num <- x
suppressWarnings(x_int <- as.integer(x))
x_char <- as.character(x)
x_lgl <- x > 0
all.equal( counts(x), c(table(x, useNA='ifany') ) )
all.equal( counts(x_int), c(table(x_int, useNA="ifany")))
all.equal(counts(x_char), c(table(x_char, useNA="ifany")))
all.equal(counts(x_lgl), c(table(x_lgl, useNA="ifany")))
if (require(microbenchmark)) {
  microbenchmark( 
    times=5, 
    counts(x_num),
    tableRcpp(x_num),
    c(table(x_num, useNA="ifany")),
    counts(x_int),
    tableRcpp(x_int),
    c(table(x_int, useNA="ifany")),
    counts(x_char),
    tableRcpp(x_char),
    c(table(x_char, useNA="ifany")),
    counts(x_lgl),
    tableRcpp(x_lgl),
    c(table(x_lgl, useNA="ifany"))
  )
}
*/

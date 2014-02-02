#ifndef KMISC_UTILS_H_
#define KMISC_UTILS_H_

template <typename T>
inline bool IsNA(T x);

template <>
inline bool IsNA< Rcpp::internal::const_string_proxy<STRSXP> >( Rcpp::internal::const_string_proxy<STRSXP> x ) {
  return x == NA_STRING;
}

template <>
inline bool IsNA<int>(int x) {
  return x == NA_INTEGER;
}

template <>
inline bool IsNA<SEXP>(SEXP x) {
  return x == NA_STRING;
}

// hacky speedups for comparing NA, NaN
// nope, x == NA_REAL does not suffice, since that always evaluates to false
template <>
inline bool IsNA<double>(double x) {
  return memcmp(
    (char*) &x,
    (char*) &NA_REAL,
    sizeof(double)
  ) == 0;
}

inline bool IsNaN(double x) {
  return memcmp(
    (char*) &x,
    (char*) &R_NaN,
    sizeof(double)
  ) == 0;
}

// borrowed from data.table package;
// see https://github.com/arunsrinivasan/datatable/blob/master/pkg/src/countingcharacter.c
inline int StrCmp(SEXP x, SEXP y)
{
    if (x == NA_STRING) return (y == NA_STRING ? 0 : 1);
    else if (y == NA_STRING) return -1;
    else if (x == y) return 0;  // same string in cache
    else return strcmp(char_nocheck(x), char_nocheck(y));
}

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

    // this branch inspired by data.table: see
    // https://github.com/arunsrinivasan/datatable/commit/1a3e476d3f746e18261662f484d2afa84ac7a146#commitcomment-4885242
    if (IsNaN(right) and IsNA(left)) return true;

    if (leftNaN != rightNaN) {
      return leftNaN < rightNaN;
    } else {
      return left < right;
    }

  }

};

template <>
struct NACompare<SEXP> {
  inline bool operator()(SEXP left, SEXP right) const {
    return StrCmp(left, right) < 0;
  }
};

template <int RTYPE, template <class> class StoragePolicy>
Rcpp::IntegerVector fastgroup(const Rcpp::Vector<RTYPE, StoragePolicy>& x) {
  typedef typename ::Rcpp::traits::storage_type<RTYPE>::type STORAGE;
  typedef typename std::set< STORAGE, NACompare<STORAGE> > SET;
  typedef typename std::set< STORAGE, NACompare<STORAGE> >::iterator ITERATOR;
  return Rcpp::wrap( set(x.begin(), x.end()) );
}

#endif

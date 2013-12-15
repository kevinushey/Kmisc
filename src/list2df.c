#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[export]]
SEXP list2df(SEXP x_, SEXP inplace) {

	SEXP x;
	int unprotect_num = 1;

	if (TYPEOF(inplace) != LGLSXP || length(inplace) > 1) {
		error("'inplace' must be a logical vector of length 1; type is '%s'",
		type2char(TYPEOF(inplace)));
	}

	if (LOGICAL(inplace)[0] < 0) {
		error("'inplace' must be non-NA");
	}

	if (TYPEOF(x_) != VECSXP)
		error("argument must be a list; type is '%s'", type2char(TYPEOF(x_)));
    
  if (inherits(x_, "data.frame"))
    warning("argument is already a data.frame");

	if (LOGICAL(inplace)[0]) {
		x = x_;
	} else {
		x = PROTECT( duplicate(x_) );
		++unprotect_num;
	}

	int m = length(x);
	int n = length( VECTOR_ELT(x, 0) );
	for (int i = 1; i < m; ++i) {
		if (length( VECTOR_ELT(x, i) ) != n) {
			error("not all columns are of equal length");
		}
	}

	SEXP row_names = PROTECT( allocVector( INTSXP, n ) );
	for (int i = 0; i < n; ++i)
		INTEGER(row_names)[i] = i + 1;

	setAttrib(x, R_ClassSymbol, mkString("data.frame"));
	setAttrib(x, R_RowNamesSymbol, row_names);

	// set the names, if NULL
	if (isNull( getAttrib(x, R_NamesSymbol) )) {
		SEXP colnames;
		PROTECT(colnames = allocVector(STRSXP, m));
		++unprotect_num;
		char str[ (int) log10(m) + 3];
    for (int i = 0; i < m; ++i) {
			sprintf(str, "%s%i", "V", i + 1);
			SET_STRING_ELT(colnames, i, mkChar(str));
		}
		setAttrib(x, R_NamesSymbol, colnames);

	}

	UNPROTECT(unprotect_num);
	return x;

}

#undef USE_RINTERNALS

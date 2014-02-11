#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[register]]
SEXP df2list(SEXP x_, SEXP inplace) {
  
  SEXP x;
  int unprotect_num = 0;

	if (TYPEOF(inplace) != LGLSXP || length(inplace) > 1) {
		error("'inplace' must be a logical vector of length 1; type is '%s'",
		type2char(TYPEOF(inplace)));
	}

	if (LOGICAL(inplace)[0] < 0) {
		error("'inplace' must be non-NA");
	}
  
  if (TYPEOF(x_) != VECSXP || !inherits(x_, "data.frame")) {
    error("argument must be a data.frame");
  }
    

	if (LOGICAL(inplace)[0]) {
		x = x_;
	} else {
		x = PROTECT( duplicate(x_) );
		++unprotect_num;
	}
  
  // strip off all attributes expect the name; make it appear
  // as a 'base' R object
  SEXP nm; 
  PROTECT(nm = duplicate( getAttrib(x, R_NamesSymbol) ));
  SET_ATTRIB(x, R_NilValue);
  setAttrib(x, R_NamesSymbol, nm);
  UNPROTECT(1);
  SET_OBJECT(x, 0);

	UNPROTECT(unprotect_num);
	return x;
  
}

#undef USE_RINTERNALS

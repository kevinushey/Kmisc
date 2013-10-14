#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

#define GUARD(X) PROTECT(X); ++numprotect;
#define UNGUARD UNPROTECT(numprotect)

SEXP unmelt(SEXP data, SEXP uniq_id, SEXP other_ind_, SEXP id_ind_, SEXP value_ind_) {

	// int id_ind = asInteger(id_ind_);
	int value_ind = asInteger(value_ind_);
	int* other_ind = INTEGER(other_ind_);
	int nRow = (int)(length(VECTOR_ELT(data, 0)) / length(uniq_id));
	int numprotect = 0;

	if (TYPEOF(uniq_id) != STRSXP) {
		GUARD(uniq_id = coerceVector(uniq_id, STRSXP));
	}

	int n_uniq = length(uniq_id);

	SEXP output;
	GUARD(output = allocVector(VECSXP, length(other_ind_) + length(uniq_id)));

	int n_other = length(other_ind_);

	// copy in the 'other' variables first
#define COPY(RTYPE, CTYPE, ACCESSOR) { \
	PROTECT(tmp = allocVector(RTYPE, nRow)); \
	CTYPE* tmp_ptr = ACCESSOR(tmp); \
	CTYPE* data_ptr = ACCESSOR(VECTOR_ELT(data, other_ind[i])); \
	for (int i=0; i < nRow; ++i) { \
		tmp_ptr[i] = data_ptr[i]; \
	} \
	SET_VECTOR_ELT(output, i, tmp); \
	UNPROTECT(1); \
	break; \
} \

	SEXP tmp;
	for (int i=0; i < n_other; ++i) {
		switch (TYPEOF(VECTOR_ELT(data, other_ind[i]))) {
		case LGLSXP: COPY(LGLSXP, int, LOGICAL);
		case INTSXP: COPY(INTSXP, int, INTEGER);
		case REALSXP: COPY(REALSXP, double, REAL);
		case STRSXP: COPY(STRSXP, SEXP, STRING_PTR);
		default: Rf_error("Unhandled SEXP type");
		}
	}

#undef COPY

#define COPY(RTYPE, CTYPE, ACCESSOR) { \
	PROTECT(tmp = allocVector(RTYPE, nRow)); \
	CTYPE* tmp_ptr = ACCESSOR(tmp); \
	CTYPE* data_ptr = ACCESSOR(VECTOR_ELT(data, value_ind)); \
	for (int j=0; j < nRow; ++j) { \
		tmp_ptr[j] = data_ptr[j + (i*nRow)]; \
	} \
	SET_VECTOR_ELT(output, i + n_other, tmp); \
	UNPROTECT(1); \
	break; \
} \

	// copy the value
	int valuetype = TYPEOF(VECTOR_ELT(data, value_ind));
	for (int i=0; i < n_uniq; ++i) {
		switch (valuetype) {
		case LGLSXP: COPY(LGLSXP, int, LOGICAL);
		case INTSXP: COPY(INTSXP, int, INTEGER);
		case REALSXP: COPY(REALSXP, double, REAL);
		case STRSXP: COPY(STRSXP, SEXP, STRING_PTR);
		}
	}

	// set the names
	SEXP datanames = getAttrib(data, R_NamesSymbol);
	SEXP names;
	GUARD(names = allocVector(STRSXP, n_other + n_uniq));
	for (int i=0; i < n_other; ++i) {
		SET_STRING_ELT(names, i, STRING_ELT(datanames, i));
	}
	for (int i=0; i < n_uniq; ++i) {
		SET_STRING_ELT(names, n_other+i, STRING_ELT(uniq_id, i));
	}
	setAttrib(output, R_NamesSymbol, names);

	// set the class
	setAttrib(output, R_ClassSymbol, mkString("data.frame"));

	// set the rows
	SEXP rownames;
	GUARD( rownames=allocVector(INTSXP, nRow) );
	int* rownames_ptr = INTEGER(rownames);
	for (int i=0; i < nRow; ++i) {
		rownames_ptr[i] = i+1;
	}
	setAttrib(output, R_RowNamesSymbol, rownames);
	UNGUARD;
	return output;
}

#undef USE_RINTERNALS

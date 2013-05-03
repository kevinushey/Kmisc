#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

SEXP simp( SEXP x, SEXP y ) {

	SEXP out;
	PROTECT( out = allocVector(REALSXP, 1) );

	int nx = length(x);
	int ny = length(y);

	if( nx != ny ) {
		Rf_error("'x' must be the same length as 'y'");
	}

	double out_num = 0;
	double mult = 1;

	for( int i=0; i < nx; ++i ) {

		// get the correct multiplier
		if( i == 0 ) {
			mult = 1.0;
		} else if( i == nx-1 ) {
			mult = 1.0;
		} else if( i % 2 == 1 ) {
			mult = 4.0;
		} else if( i % 2 == 0 ) {
			mult = 2.0;
		}

		out_num = out_num + (mult * REAL(y)[i]);
		// Rprintf("i = %i; REAL(y)[i] = %f; mult = %f; out_num = %f\n", i, REAL(y)[i], mult, out_num);
	}

	double h = (REAL(x)[nx-1] - REAL(x)[0]) / (double) nx;
	// Rprintf("h = %f\n", h);

	out_num = (h / 3.0) * out_num;
	// Rprintf("the final value of out_num is %f\n", out_num);

	REAL(out)[0] = out_num;
	UNPROTECT(1);
	return out;

}

#undef USE_RINTERNALS

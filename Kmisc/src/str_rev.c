#include <R.h>
#include <Rdefines.h>

SEXP str_rev( SEXP x ) {
  
  int len = Rf_length(x);
  SEXP out;
  PROTECT( out = allocVector( STRSXP, len ) );
  
  // Loop through each string
  for( int i=0; i < len; i++ ) {
    
    // Get the current element of the string
    int len_elt = Rf_length( STRING_ELT(x, i) );
    const char* element = CHAR( STRING_ELT(x, i) );
    
    // Allocate space for the reversed string
    char* elt_rev = malloc( sizeof(char) * (len_elt + 1) );
    
    // Reverse 'elt'
    for( int j=0; j < len_elt; j++ ) {
      elt_rev[j] = element[ len_elt - j - 1];
    }
    
    // Set the null terminator
    elt_rev[len_elt] = '\0';
    
    // Set the i'th element of out to the reversed char
    SET_STRING_ELT( out, i, mkChar( elt_rev ) );
    
    // free the memory allocated for elt_rev
    free( elt_rev );
    
  }
    
    UNPROTECT(1);
    return out;
}
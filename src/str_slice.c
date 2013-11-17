#define USE_RINTERNALS

#include <R.h>
#include <Rdefines.h>

// [[export]]
SEXP str_slice(SEXP x, SEXP n) {
    
    // Treat x as a vector of characters
    int x_len = length(x);
    int len_substr = INTEGER(n)[0];
    
    // Allocate memory for a list
    SEXP out;
    PROTECT( out = allocVector(VECSXP, x_len) );
    
    for( int k=0; k < x_len; ++k ) {
        
        // The string as a pointer to an array of characters
        const char* xx = CHAR(STRING_ELT( x, k ) );

        // The length of the string supplied
        int len = length( STRING_ELT( x, k ) );

        // The number of substrings
        int num_substr = len / len_substr;
        
        // Allocate memory for the vector of substrings
        SEXP substring;
        PROTECT( substring = allocVector(STRSXP, num_substr) );

        int string_counter = 0;
        for( int i=0; i < num_substr; ++i ) {

            // allocate memory for a string
            char* elt = R_alloc( len_substr+1, sizeof(char)  );

            // Push items onto the element
            for( int j=0; j < len_substr; ++j ) {
                elt[j] = xx[string_counter];
                string_counter++;
            }

            // Set the terminator
            elt[len_substr] = '\0';

            SET_STRING_ELT( substring, i, mkChar(elt) );
        }
        
        // Set the list element to the substring
        SET_VECTOR_ELT(out, k, substring);
        UNPROTECT(1);
        
    }
    
    UNPROTECT(1);
    return( out );
    
}

#undef USE_RINTERNALS

#include <Rcpp.h>

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace std;
using namespace Rcpp;

// strslice function

RcppExport SEXP strslice(SEXP x, SEXP n) {
    BEGIN_RCPP

    std::string myString = as<std::string>(x);
    int cutpoint = as<int>(n);
    int len = myString.length();
    int nout = len / cutpoint ;
    CharacterVector out( nout ) ;
    for( int i=0; i<nout; i++ ) {
      out[i] = myString.substr( cutpoint*i, cutpoint ) ;
    }
    return out;

    END_RCPP
}

RcppExport SEXP count_nucleotides(SEXP x) {
    BEGIN_RCPP
            
    IntegerVector counts(4);
    
    counts[0] = 0;
    counts[1] = 0;
    counts[2] = 0;
    counts[3] = 0;
    
    std::string myString = as<std::string>(x);
    for( unsigned int i=0; i < myString.length(); i++ ) {
        if( myString[i] == 'A' ) counts[0] = counts[0] + 1;
        if( myString[i] == 'C' ) counts[1] = counts[1] + 1;
        if( myString[i] == 'G' ) counts[2] = counts[2] + 1;
        if( myString[i] == 'T' ) counts[3] = counts[3] + 1;
    }
    
    return Rcpp::wrap(counts);
    
    END_RCPP
}
RcppExport SEXP transcribe_RNA(SEXP x) {
    BEGIN_RCPP
            
    std::string myString = as<std::string>(x);
    for( unsigned int i=0; i < myString.length(); i++ ) {
        if( myString[i] == 'T' ) myString[i] = 'U';
    }
    
    return Rcpp::wrap(myString);
    
    END_RCPP
}

RcppExport SEXP failure_array( SEXP x ) {
    BEGIN_RCPP
    
    string W = as<string>(x);
    IntegerVector T(W.length());
    
    unsigned int pos = 1;
    int cnd = 0;
    
    T[0] = 0;
    
    while( pos < W.length() ) {
        if( W[pos] == W[cnd] ) {
            cnd = cnd + 1;
            T[pos] = cnd;
            pos = pos + 1;
        } else if( cnd > 0 ) {
            cnd = T[cnd-1];
        } else {
            T[pos] = 0;
            pos = pos + 1;
        }
    }
    
    return wrap(T);
    
    END_RCPP
}

RcppExport SEXP kRev( SEXP x ) {
    BEGIN_RCPP
    
    Vector<VECSXP> xx(x);
        
    StringVector out(xx.size());
    
    for( int i=0; i < xx.size(); i++ ) {
        string tmp = as<string>(xx[i]);
        reverse( tmp.begin(), tmp.end() );
        out[i] = tmp;
    }
                
    return( wrap( out ) );
            
    END_RCPP
            
}
##' Split by Runs
##' 
##' Split a vector into a list of runs, such that each entry in the
##' output list is a set of runs encountered. This function accepts two forms
##' of inputs: either a vector where each element of the vector is of length
##' 1 (e.g. \code{c("A", "A", "C", "T")}), or a vector of length 1 interpretted
##' as a long string (e.g. \code{"AAAACCAGGGACGCCGCGGTTGG"}).
##' 
##' Factors will be coerced to character before splitting.
##' 
##' @param x A numeric or character vector.
##' @export
##' @seealso \code{\link{rle}}, for a similar function with different output.
##' @examples
##' x <- rbinom( 100, 2, 0.5 )
##' stopifnot( all( x == unlist( split_runs(x) ) ) )
##' stopifnot( all( as.character(x) == unlist( split_runs( as.character(x) ) ) ) )
##' y <- paste( collapse="", sample( LETTERS[1:5], 1E5, replace=TRUE ) )
##' stopifnot( y == paste( collapse="", split_runs(y) ) )
##' z <- replicate( 25, paste( collapse="", sample( LETTERS[1:5], 1E3, replace=TRUE ) ) )
##' system.time( lapply(z, split_runs) )
split_runs <- function(x) {
  
  if (is.factor(x)) {
    x <- factor_to_char(x)
  }
  
  if( length(x) == 1 ) {
    return( .Call(CKmisc_split_runs_one, as.character(x)) )
  }
  
  if( is.character(x) ) {
    return( .Call(CKmisc_split_runs_character, x) )
  }
  
  if( is.numeric(x) ) {
    return( .Call(CKmisc_split_runs_numeric, x) )
  }
  
  stop("x is of incompatible type")
  
}

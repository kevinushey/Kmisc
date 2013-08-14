##' Count Matches
##' 
##' This function returns a matrix of matches between each argument passed.
##' 
##' @param ... A set of (possibly named) arguments, all of the same type.
##' @export
matches <- function(...) {
  output <- .Call( "Kmisc_matches", list(...), PACKAGE="Kmisc" )
  rownames(output) <- colnames(output) <- names( list(...) )
  return(output)
}

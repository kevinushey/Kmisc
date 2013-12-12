##' Count Matches
##' 
##' This function returns a matrix of matches between each argument passed.
##' Each cell \code{x_ij} in the output denotes how many times the elements in
##' input \code{i} were found in input \code{j}.
##' 
##' @param ... A set of (possibly named) arguments, all of the same type.
##' @export
##' @examples
##' x <- c("a", "b", "c", "d")
##' y <- c("a", "b", "c")
##' z <- c("a", "b", "d")
##' matches(x, y, z)
matches <- function(...) {
  output <- .Call( CKmisc_matches, list(...) )
  rownames(output) <- colnames(output) <- names( list(...) )
  return(output)
}

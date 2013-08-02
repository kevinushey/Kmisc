##' Generate a Named Vector
##' 
##' Generate a named vector.
##' 
##' @param x A vector.
##' @param names A vector of names.
##' @export
named <- function(x, names) {
  names(x) <- names
  return(x)
}

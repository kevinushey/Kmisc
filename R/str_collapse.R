##' Collapse a String
##' 
##' This function collapses a string using Rcpp sugar, and
##' operates similarily to \code{paste0(..., collapse="")}.
##' 
##' @param x A list of character vectors.
##' @export
str_collapse <- function(x) {
  return(.Call("Kmisc_str_collapse", as.list(x)))
}

##' Collapse a String
##' 
##' This function collapses a string using Rcpp sugar, and
##' operates similarily to \code{paste0(..., collapse="")}.
##' 
##' @param x A list of character vectors.
##' @export
str_collapse <- function(x) {
  
  if (is.list(x)) {
    return(.Call(CKmisc_str_collapse_list, x))
  } else {
    return(.Call(CKmisc_str_collapse_str, as.character(x)))
  }
  
}

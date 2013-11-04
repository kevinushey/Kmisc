##' Python-style Formatting of Strings.
##' 
##' This function allows Python-style formatting of strings, whereby text of
##' the form \code{{0}, {1}, ..., {n}} is substituted according to the
##' matching argument passed to \code{...}. \code{0} corresponds to the
##' first argument, \code{1} corresponds to the second, and so on.
##' 
##' @param x A string with arguments to be replaced in the form of
##'   \code{{0}, {1}, ..., {n}}.
##' @param ... Arguments to be substituted into \code{x}.
##' @param collapse If vectors of length greater than 1 are passed to \code{...},
##'   then we collapse the vectors with this separator.
##' @export
##' @examples
##' pymat(
##'   "My favourite fruits are: {0}, {1}, and {2}.", 
##'   "apple", "banana", "orange"
##' )
##' 
##' pymat(
##'   "My favourite fruits are: {0}.", 
##'   c("apple", "banana", "orange"), collapse=", "
##' )
pymat <- function(x, ..., collapse=", ") {
  
  ## single strings only
  if( length(x) > 1 ) {
    stop("'x' must be a single string")
  }
  
  ## collapse vectors passed to dot args
  dotArgs <- sapply( list(...), paste, collapse=collapse )
  
  i <- 0
  for( arg in dotArgs ) {
    to_replace <- paste( sep='', "{", i, "}" )
    x <- gsub( to_replace, arg, x, fixed=TRUE )
    i <- i + 1
  }
  return( x )
}

##' Wrap a String
##' 
##' This function operates similarily to \code{\link{strwrap}}, but
##' \code{paste}s the wrapped text back together with line separators.
##' Useful for automatically wrapping long labels.
##' 
##' @param x A character vectors, or an object which can be converted to
##'   a character vector by \code{\link{as.character}}.
##' @param width A positive integer giving the number of characters a line
##'   can reach before we wrap and introduce a new line.
##' @param ... Optional arguments passed to \code{\link{strwrap}}.
##' @examples
##' long_label <- "This is a very long label which needs wrapping."
##' wrap(long_label)
##' @export
wrap <- function(x, width=8, ...) {
  return( paste( strwrap(x, width, ...), collapse="\n" ) )
}

##' Utility Function for Citing a Package
##'
##' We pull the text version out of the citation object returned
##' by \code{citation()}.
##'
##' @param ... A (set of) package names.
##' @export
cite_package <- function(...) {
  args <- unlist( list(...) )
  cites <- lapply(args, citation)
  txt <- sapply(cites, function(x) {
    attr( unclass(x)[[1]], "textVersion" )
  })
  return(txt[order(txt)])
}

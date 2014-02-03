##' Fast Factor Generation
##' 
##' This function generates factors more quickly, by leveraging
##' \code{fastmatch::\link{fmatch}}.
##' 
##' \code{NaN}s are converted to \code{NA} when used on numerics.
##' 
##' @importFrom data.table setattr
##' @importFrom fastmatch fmatch
##' @param x An object of atomic type \code{integer}, \code{numeric},
##'   \code{character} or \code{logical}.
##' @param levels An optional character vector of levels. Is coerced to the same type as
##'   \code{x}. By default, we compute the levels as \code{sort(unique.default(x))}.
##' @export
factor_ <- function(x, levels=NULL) {
  
  if (is.factor(x)) return(x)
  if (is.null(levels)) levels <- sort(unique.default(x))
  suppressWarnings(f <- fmatch(x, levels))
  levels(f) <- as.character(levels)
  class(f) <- "factor"
  f
  
}

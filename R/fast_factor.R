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
##' @param labels A set of labels used to rename the levels, if desired.
##' @param na.last If \code{TRUE} and there are missing values, the last level is
##'   set as \code{NA}; otherwise; they are removed.
##' @export
factor_ <- function(x, levels=NULL, labels=levels, na.last=NA) {
  
  if (is.factor(x)) return(x)
  if (is.null(levels)) levels <- sort(unique.default(x), na.last=na.last)
  suppressWarnings(f <- fmatch(x, levels, nomatch=if (isTRUE(na.last)) length(levels) else NA_integer_))
  levels(f) <- as.character(labels)
  class(f) <- "factor"
  f
  
}

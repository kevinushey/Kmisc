##' Faster tapply
##' 
##' This function acts as a faster version of \code{tapply} for the common case of
##' splitting an atomic vector by another atomic vector, and then applying a
##' function.
##' 
##' @param X An atomic vector.
##' @param INDEX A vector coercable to factor; must be one of the common atomic types:
##' factor, integer, numeric, or character.
##' @param FUN The function to be applied. See more details at \code{\link{lapply}}.
##' @param FUN.VALUE Optional; if specified we try to leverage \code{vapply} for
##' computation of results.
##' @param ... Optional arguments to pass to \code{FUN}.
##' @param simplify boolean; if \code{TRUE}, we unlist the output and hence return
##' a named vector of values.
##' @param USE.NAMES boolean; if \code{TRUE} use \code{X} as \code{\link{names}} for
##' the result unless it had names already. Sometimes, one can achieve substantial
##' speedups by setting this to \code{FALSE}. This option is only used when
##' \code{FUN.VALUE} is not \code{NULL}.
##' @param na.last Boolean, if \code{TRUE} \code{NA} values are grouped at the end.
##'   Ie, we group on the \code{NA}s as well.
##' @export
##' @examples
##' x <- rnorm(100)
##' gp <- sample( 1:10, 100, TRUE )
##' stopifnot( all(
##'   tapply(x, gp, mean) == tapply_(x, gp, mean)
##' ) )
tapply_ <- function(X, INDEX, FUN=NULL, FUN.VALUE=NULL, ...,
  simplify=TRUE, USE.NAMES=TRUE, na.last=TRUE) {
  
  na.last <- if (isTRUE(na.last)) TRUE else NA
  
  if (is.null(FUN.VALUE)) {
    output <- lapply(
      X=split_(X, INDEX, na.last=na.last),
      FUN=FUN,
      ...
    )
    if (simplify) return (unlist(output))
    else return (output)
  } else {
    return( vapply( 
      X=split_(X, INDEX, na.last=na.last),
      FUN=FUN,
      FUN.VALUE=FUN.VALUE,
      USE.NAMES=USE.NAMES, 
      ...
    ) )
  }
  
}


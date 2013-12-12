##' Tranpose an Object
##' 
##' This functions similarily to \R's \code{t}, but we add a new method,
##' \code{transpose.list}, for transposing lists in a specific way.
##' 
##' @param x A matrix, data.frame, or matrix-like list.
##' @export
transpose <- function(x) {
  UseMethod("transpose")
}

##' @rdname transpose
##' @S3method transpose list
##' @method transpose list
transpose.list <- function(x) {
  return( .Call(Ctranspose_list, as.list(x)) )
}

##' @rdname transpose
##' @S3method transpose data.frame
##' @method transpose data.frame
transpose.data.frame <- function(x) {
  return( t.data.frame(x) )
}

##' @rdname transpose
##' @S3method transpose default
##' @method transpose default
transpose.default <- function(x) {
  return( t.default(x) )
}

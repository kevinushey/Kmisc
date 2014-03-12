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
##' @export
transpose.list <- function(x) {
  return( .Call(Ctranspose_list, as.list(x)) )
}

##' @rdname transpose
##' @export
transpose.data.frame <- function(x) {
  return( as.matrix( transpose.list(x) ) )
}

##' @rdname transpose
##' @export
transpose.default <- function(x) {
  return( t.default(x) )
}

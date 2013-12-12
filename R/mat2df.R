##' Convert a Matrix to a DataFrame
##' 
##' Identical to \code{as.matrix.data.frame}, but faster.
##' 
##' @param x A \code{matrix}.
##' @export
mat2df <- function(x) {
  return(.Call(Cmat2df, x))
}

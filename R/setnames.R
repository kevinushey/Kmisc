##' Set the row, column names of a matrix in place
##' 
##' Similar to the \code{setattr} or \code{setnames} functions of
##' \code{data.table}, but makes it slightly easier to set these attributes
##' in-place for matrices.
##' 
##' @rdname setnames
##' @param x Either a \code{data.frame}, or an array.
##' @param value Either \code{NULL} or a vector coercible to character.
##' @export
setrownames <- function(x, value) {
  .Call(Csetrownames, x, value)
}

##' @rdname setnames
##' @export
setcolnames <- function(x, value) {
  .Call(Csetcolnames, x, value)
}

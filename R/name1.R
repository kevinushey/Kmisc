##' Set the NAMED attribute
##' 
##' This function sets the \code{NAMED} attribute of an \R object to
##' one. For development and debugging purposes only.
##' 
##' @param x An \R object.
##' @param i An integer equal to 0, 1, or 2.
setnamed <- function(x, i) {
  if (!any(
    identical(i, 0L),
    identical(i, 1L),
    identical(i, 2L),
    identical(i, 0),
    identical(i, 1),
    identical(i, 2)
  )) stop("i must be an integer equal to 0, 1, or 2")
  
  .Call(Csetnamed, x, as.integer(i))
  invisible(NULL)
}

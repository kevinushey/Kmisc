##' Set the NAMED attribute
##' 
##' This function sets the \code{NAMED} attribute of an \R object.
##' For development and debugging purposes only.
##' 
##' @param x An \R object.
##' @param i An integer equal to 1 or 2.
setnamed <- function(x, i) {
  i <- as.integer(i)
  if (!(i %in% c(1:2))) {
    stop("i must be an integer equal to 1 or 2")
  }
  .Call(Csetnamed, x, as.integer(i))
  invisible(NULL)
}

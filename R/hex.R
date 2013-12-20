##' Get the hex representation of a value
##' 
##' @param x A value.
##' @export
hex <- function(x) {
  UseMethod("hex")
}

##' @export
hex.double <- function(x) {
  .Call(Cdouble2hex, x)
}

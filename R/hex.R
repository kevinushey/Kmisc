##' Get the hex representation of a value
##' 
##' Prints the hex (byte) representation of a value.
##' 
##' @param x A value.
##' @export
hex <- function(x) {
  UseMethod("hex")
}

##' @export
hex.double <- function(x) {
  vapply(x, FUN.VALUE=character(1), function(xx) .Call(Cdouble2hex, xx) )
}

##' @export
hex.integer <- function(x) {
  vapply(x, FUN.VALUE=character(1), function(xx) .Call(Cint2hex, xx) )
}

##' @export
hex.logical <- function(x) {
  vapply(x, FUN.VALUE=character(1), function(xx) .Call(Cint2hex, xx) )
}

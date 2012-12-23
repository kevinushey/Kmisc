#' Reverse a Vector of Strings
#' 
#' Reverses a vector of 'strings' (a character vector).
#' 
#' @export
#' @param x a character vector
str_rev <- function(x) {
  .Call("str_rev", as.character(x), PACKAGE="Kmisc")
}
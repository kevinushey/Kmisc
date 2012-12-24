#' Reverse a Vector of Strings
#' 
#' Reverses a vector of 'strings' (a character vector).
#' 
#' @export
#' @param x a character vector
#' @note Underlying code is written in C for fast execution.
str_rev <- function(x) {
  tmp <- .Call("str_rev", as.character(x), PACKAGE="Kmisc")
  tmp[ is.na(x) ] <- NA
  return(tmp)
}
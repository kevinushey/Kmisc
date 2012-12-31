#' Sort a Vector of Strings
#' 
#' Sorts a vector of strings lexically, as based on their 
#' UTF-8 ordering scheme. Lower-case letters are, by default, 
#' 'larger' than upper-case letters.
#' 
#' The sorting is done in C++ with \code{std::sort}.
#' 
#' @export
#' @param x a character vector (a vector of 'strings' to sort)
#' @param increasing boolean. sort the string in increasing lexical order?
#' @examples
#' stopifnot( all( str_sort(c("cba", "fed")) == c("abc", "def") ) )
str_sort <- function(x, increasing=TRUE) {
  .Call( "str_sort", 
         as.character(x), 
         as.logical(increasing),
         PACKAGE="Kmisc" 
  )
}
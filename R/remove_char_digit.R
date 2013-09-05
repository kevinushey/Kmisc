##' Remove Alphabetic Characters from a Character Vector
##' 
##' Removes all alphabetic characters from a character vector.
##' 
##' @param x A character vector, or vector coercable to character.
##' @param remove_spaces boolean; if \code{TRUE} we remove all white-space as
##' well.
##' @export
remove_chars <- function(x, remove_spaces=TRUE) {
  
  if( !is.character(x) ) {
    x <- as.character(x)
  }
  
  tmp <- gsub( "[A-z]", "", x )
  if( remove_spaces ) tmp <- gsub( "[[:blank:]]", "", tmp )
  return( tmp )
}

##' Remove Digits from a Character Vector
##' 
##' Removes all digits from a character vector.
##' 
##' @param x A character vector, or vector coercable to character.
##' @param remove_spaces boolean; if \code{TRUE} we remove all white-space as
##' well.
##' @export
remove_digits <- function(x, remove_spaces=TRUE) {
  
  if( !is.character(x) ) {
    x <- as.character(x)
  }
  
  tmp <- gsub( "[[:digit:]]", "", x )
  if( remove_spaces ) tmp <- gsub( "[[:space:]]", "", tmp )
  return( tmp )
  
}

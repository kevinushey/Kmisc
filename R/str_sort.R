.letters.UTF8 <- sapply( letters, utf8ToInt )
.LETTERS.UTF8 <- sapply( LETTERS, utf8ToInt )

.letters.from <- c( rbind( .letters.UTF8, .LETTERS.UTF8 ) )
.letters.to <- c( cbind( .letters.UTF8, .LETTERS.UTF8 ) )

##' Sort a Vector of Strings
##' 
##' Sorts a vector of strings lexically, as based on their 
##' UTF-8 ordering scheme. Lower-case letters are, by default, 
##' 'larger' than upper-case letters. This function will safely sort a
##' UTF-8 vector.
##' 
##' @export
##' @param x a character vector (a vector of 'strings' to sort)
##' @param increasing boolean. sort the string in increasing lexical order?
##' @param ignore.case boolean. ignore case (so that, eg, \code{a < A < b})
##' @param USE.NAMES logical. if names attribute already exists on \code{x},
##' pass this through to the result?
##' @examples
##' stopifnot( all( str_sort(c("cba", "fed")) == c("abc", "def") ) )
str_sort <- function(x, increasing=TRUE, ignore.case=FALSE, USE.NAMES=FALSE) {
  vapply( x, USE.NAMES=USE.NAMES, FUN.VALUE="character", function(xx) {
    tmp <- utf8ToInt(xx)
    if( ignore.case ) {
      tmp <- swap( tmp, .letters.from, .letters.to )
      tmp <- sort( tmp, decreasing=!increasing )
      tmp <- swap( tmp, .letters.to, .letters.from )
    } else {
      tmp <- sort( tmp, decreasing=!increasing )
    }
    return( intToUtf8(tmp) )
  })
}

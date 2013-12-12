##' Reverse a Vector of Strings
##' 
##' Reverses a vector of 'strings' (a character vector). Not safe for
##' unicode (UTF-8) characters.
##' 
##' This function is written in C for fast execution; however, we do not handle 
##' non-ASCII characters. For a 'safe' version of \code{str_rev} that handles
##' unicode characters, see \code{\link{str_rev2}}.
##' @export
##' @param x a character vector.
##' @seealso \code{\link{str_rev2}}
##' @examples
##' x <- c("ABC", "DEF", "GHIJ")
##' str_rev(x)
str_rev <- function(x) {
  tmp <- .Call(Cstr_rev, as.character(x))
  tmp[ is.na(x) ] <- NA
  return(tmp)
}

##' Reverse a Vector of Strings (UTF-8)
##' 
##' Reverses a vector of 'strings' (a character vector). This will safely reverse a
##' vector of unicode (UTF-8) characters.
##' 
##' This function will handle UTF-8 characters safely. If you
##' are working only with ASCII characters and require speed, 
##' see \code{\link{str_rev2}}.
##' 
##' @export
##' @param x a character vector.
##' @seealso \code{\link{str_rev}}
##' @examples
##' x <- c("ABC", "DEF", "GHIJ")
##' str_rev(x)
str_rev2 <- function(x) {
  vapply( as.character(x), 
          USE.NAMES=FALSE, 
          FUN.VALUE="character",
          function(xx) {
            intToUtf8( rev( utf8ToInt( xx ) ) )
          })
}

##' Remove Elements from a Named Object
##' 
##' Removes elements from an \R object
##' with the names attribute set in a 'lazy' way. 
##' The first argument is the object, while the second is a set of names 
##' parsed from \code{...}. We return the object, including 
##' only the elements with names not matched in \code{...}.
##' 
##' We can be 'lazy' with how we pass names. The \code{\link{name}}s
##' passed to \code{...} are not evaluated directly; rather, their character
##' representation is taken and used for extraction.
##' 
##' @param x An \R object with a \code{names} attribute.
##' @param ... an optional number of 'names' to match in \code{x}.
##' @export
##' @seealso \code{\link{extract}}
##' @examples
##' dat <- data.frame( x=c(1, 2, 3), y=c("a", "b", "c"), z=c(4, 5, 6) )
##' ## all of these return identical output
##' dat[ !( names(dat) %in% c("x","z") ) ]
##' without(dat, x, z)
without <- function(x, ...) {
  args <- as.character(match.call(expand.dots=FALSE)$`...`)
  missing_names <- args[ args %nin% names(x) ]
  if (length(missing_names)) {
    warning("The following names were not found in '", deparse(substitute(x)), 
      "':\n\t", paste(args[args %nin% names(x)], collapse=", "))
  }
  return(x[ names(x) %nin% args ])
}

##' Extract Elements from a Named Object
##' 
##' Extracts elements from an \R object
##' with the names attribute set in a 'lazy' way. 
##' The first argument is the object, while the second is a set of names 
##' parsed from \code{...}. We return the object, including 
##' only the elements with names matched from \code{...}.
##' 
##' We can be 'lazy' with how we pass names. The \code{\link{name}}s
##' passed to \code{...} are not evaluated directly; rather, their character
##' representation is taken and used for extraction.
##' 
##' @param x An \R object with a \code{names} attribute.
##' @param ... an optional number of 'names' to match in \code{dat}.
##' @export
##' @seealso \code{\link{without}}, \code{\link{extract.re}}
##' @examples
##' dat <- data.frame( x = c(1, 2, 3), y = c("a", "b", "c"), z=c(4, 5, 6) )
##' ## all of these return identical output
##' dat[ names(dat) %in% c("x","z") ]
##' extract( dat, x, z)
extract <- function( x, ... ) {
  
  args <- as.character(match.call(expand.dots=FALSE)$`...`)
  missing_names <- args[ args %nin% names(x) ]
  if (length(missing_names)) {
    warning("The following names were not found in '", deparse(substitute(x)), 
      "':\n\t", paste(args[args %nin% names(x)], collapse=", "))
  }
  return(x[ names(x) %in% args ])
  
}

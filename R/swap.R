##' Swap Elements in a Vector
##' 
##' This function swaps elements in a vector. See examples for usage.
##' 
##' If \code{to} is of different type than \code{from}, it will be
##' coerced to be of the same type.
##' 
##' @param vec the vector of items whose elements you will be replacing.
##' @param from the items you will be mapping 'from'.
##' @param to the items you will be mapping 'to'. must be same length and
##' order as \code{from}.
##' @export
##' @seealso \code{\link{swap_}}
##' @examples
##' x <- c(1, 2, 2, 3)
##' from <- c(1, 2)
##' to <- c(10, 20)
##' swap( x, from, to )
##' 
##' ## alternatively, we can submit a named character vector
##' ## we translate from value to name. note that this forces
##' ## a conversion to character
##' names(from) <- to
##' swap( x, from )
##' 
##' ## NAs are handled sensibly. Types are coerced as needed.
##' x <- c(1, NA, 2, 2, 3)
##' swap(x, c(1, 2), c("a", "b") )
##' 
swap <- function( vec, from, to=names(from) ) {
  return( .swap(vec, from, to) )
}

##' Swap Elements in a Vector
##' 
##' This function swaps elements in a vector. See examples for usage.
##' 
##' If \code{to} is of different type than \code{from}, it will be coerced
##' to be of the same type.
##' 
##' @param vec the vector of items whose elemetns you will be replacing.
##' @param ... A set of named arguments, whereby we translate from \code{names}
##' to \code{values} of those arguments.
##' @seealso \code{\link{swap}}
##' @export
##' @examples
##' x <- c('a', 'a', 'b', 'c')
##' swap_(x, a="A")
swap_ <- function(vec, ...) {
  dotArgs <- match.call(expand.dots=FALSE)$`...`
  return( .swap(
    vec, 
    names(dotArgs),
    unlist(dotArgs)
  ) )
}

.swap <- function(vec, from, to) {
  tmp <- to[match(vec, from)]
  tmp[is.na(tmp)] <- vec[is.na(tmp)]
  return(tmp)
}

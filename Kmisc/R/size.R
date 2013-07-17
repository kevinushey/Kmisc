#' Print the Object Size, with Auto Units
#' 
#' Provides an estimate of the memory that is being used to store an \R
#' object. Similar to \code{\link{object.size}}, but we set \code{units="auto"}
#' as default.
#' 
#' @param x An \R object.
#' @param quote logical, indicating whether or not the result should be printed
#'   with surrounding quotes.
#' @param units The units to be used in printing the size. Other allowed values are
#' \code{"Kb"}, \code{"Mb"}, \code{"Gb"} and \code{"auto"}. See \code{\link{object.size}}
#' for more details.
#' @param ... Arguments to be passed to or from other methods.
#' @export
size <- function(x, quote=FALSE, units="auto", ...) {
  m <- object.size(x)
  print( m, quote=quote, units=units, ... )
  return( invisible(m) )
}

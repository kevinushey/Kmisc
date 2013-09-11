##' Print the Object Size, with Auto Units
##' 
##' Provides an estimate of the memory that is being used to store an \R
##' object. Similar to \code{\link{object.size}}, but we set \code{units="auto"}
##' as default.
##' 
##' @param x An \R object.
##' @param quote logical, indicating whether or not the result should be printed
##'   with surrounding quotes.
##' @param units The units to be used in printing the size. Other allowed values are
##' \code{"Kb"}, \code{"Mb"}, \code{"Gb"} and \code{"auto"}. See \code{\link{object.size}}
##' for more details.
##' @param ... Arguments to be passed to or from other methods.
##' @export
size <- function(x, quote=FALSE, units="auto", ...) {
  m <- object.size(x)
  .size(m, quote=quote, units=units, ...)
  return( invisible(m) )
}

## copied from utils:::print.object_size
.size <- function(x, quote = FALSE, units = "b", ...) {
  units <- match.arg(units, c("b", "auto", "Kb", "Mb", "Gb", 
    "B", "KB", "MB", "GB"))
  if (units == "auto") {
    if (x >= 1024^3) 
      units <- "Gb"
    else if (x >= 1024^2) 
      units <- "Mb"
    else if (x >= 1024) 
      units <- "Kb"
    else units <- "b"
  }
  y <- switch(units, b = , B = paste(x, "bytes"), Kb = , KB = paste(round(x/1024, 
    1L), "Kb"), Mb = , MB = paste(round(x/1024^2, 1L), "Mb"), 
    Gb = , GB = paste(round(x/1024^3, 1L), "Gb"))
  if (quote) 
    print.default(y, ...)
  else cat(y, "\n", sep = "")
  invisible(x)
}

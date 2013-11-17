##' Generate a Sequence of Integers, and Split into Chunks
##' 
##' This function takes a set of endpoints, and 'chunks' the sequence from
##' \code{min} to \code{max} into a list with each element of size \code{size}.
##' 
##' If \code{max} is not specified, then we generate a chunk of integers
##' from 1 to \code{min}, each of size \code{size}. This allows you to
##' specify chunks with syntax like \code{chunk(100, size=5)}.
##' 
##' @param min The lower end (start point) of the sequence.
##' @param max The upper end (end point) of the sequence.
##' @param size The number of elements to place in each chunk.
##' @param by The difference between consecutive elements.
##' @export
chunk <- function(min, max, size, by=1) {
  if( missing(max) ) {
    max <- min
    min <- 1
  }
  n <- ceiling( (max-min) / (size*by) )
  out <- vector("list", n)
  lower <- min
  upper <- by * (min + size - 1)
  for( i in 1:length(out) ) {
    out[[i]] <- seq(lower, min(upper, max), by=by)
    lower <- lower + size * by
    upper <- upper + size * by
  }
  names(out) <- paste(sep="", "chunk", 1:length(out))
  return(out)
}

##' Force a Copy of an R Object
##' 
##' In \R, objects are copied 'lazily'. We use this function to force a copy.
##' 
##' @param x An \R object.
##' @export
duplicate <- function(x) {
  return( .Call(Ccopy, x) )
}

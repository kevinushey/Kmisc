##' Force a Copy of an R Object
##' 
##' In \R, things are copied 'lazily'. We use this function to force a copy
##' in case it's required.
##' 
##' @param x An \R object.
##' @export
duplicate <- function(x) {
  return( .Call("copy", x, PACKAGE="Kmisc") )
}

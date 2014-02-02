##' Fast Split
##' 
##' @export
split_ <- function(x, f) {
  if (is.character(x))
    return( split(x, factor_(f)) )
  else
    return( .Call(CKmisc_split, x, f) )
}

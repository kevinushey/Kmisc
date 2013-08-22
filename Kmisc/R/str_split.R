##' Split a Vector of Strings Following a Regular Structure
##' 
##' This function takes a vector of strings following a regular 
##' structure, and converts that vector into a \code{data.frame}, split
##' on that delimiter. A nice wrapper to \code{\link{strsplit}}, essentially 
##' - the primary bonus is the automatic coersion to a \code{data.frame}.
##' @param x a vector of strings.
##' @param sep the delimiter / \code{\link{regex}} you wish to split your strings on.
##' @param fixed logical. If \code{TRUE}, we match \code{sep} exactly; 
##' otherwise, we use regular expressions. Has priority over \code{perl}.
##' @param perl logical. Should perl-compatible regexps be used? Ignored when
##' \code{fixed} is \code{TRUE}.
##' @param useBytes logical. If \code{TRUE}, matching is done byte-by-byte rather than
##' character-by-character.
##' @param names optional: a vector of names to pass to the returned \code{data.frame}.
##' @seealso \code{\link{strsplit}}
##' @export
##' @examples
##' str_split( 
##'   c("regular_structure", "in_my", "data_here"), 
##'   sep="_", 
##'   names=c("apple", "banana") 
##' )
##' x <- c("somewhat_different.structure", "in_this.guy")
##' str_split( x, "[_\\.]", names=c("first", "second", "third") )
str_split <- function(x, sep, fixed=FALSE, perl=TRUE, useBytes=FALSE, names=NULL) {
  
  if( fixed ) perl <- FALSE
  
  x <- as.character(x)
  tmp <- strsplit( x, sep, fixed=fixed, perl=perl, useBytes=useBytes )
  if( length( unique( sapply( tmp, length ) ) ) > 1 ) {
    stop("non-equal lengths for each entry of x post-splitting")
  }
#   tmp <- unlist(tmp)
#   tmp <- as.data.frame( matrix( tmp, ncol=length(tmp)/length(x), byrow=TRUE ),
#                         stringsAsFactors=FALSE, optional=TRUE
#   )
  tmp <- .Call( "charlist_transpose_to_df", tmp, PACKAGE="Kmisc" )
  if( !is.null(names) ) {
    names(tmp) <- names
  } else {
    names(tmp) <- paste( "V", 1:ncol(tmp), sep="" )
  }
  
  return(tmp)
  
}

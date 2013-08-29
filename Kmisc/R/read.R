##' Read a File
##' 
##' These functions read a file in. We memory map the file for fast I/O.
##' The file is read in as a character vector (length one for \code{read},
##' length \code{n} for \code{readlines}).
##'  
##' @rdname read
##' @export
read <- function(file) {
  .Call("Kmisc_read", as.character(file), FALSE, PACKAGE="Kmisc")
}

##' @rdname read
##' @param file Path to a file.
##' @export
##' @examples
##' p <- file.path( R.home(), "NEWS" )
##' stopifnot( identical( readLines(p), readlines(p) ) )
readlines <- function(file) {
  .Call("Kmisc_read", as.character(file), TRUE, PACKAGE="Kmisc")
}

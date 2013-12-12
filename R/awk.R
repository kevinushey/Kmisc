.Kmisc$awk <- "awk"

##' A Simple Front-end to Awk
##' 
##' This function provides a simple front-end to \code{awk}. It assumes that
##' you have \code{awk} available and in your \code{PATH}.
##' 
##' @param code The \code{awk} code you want to put in the main execution block.
##' @param file The file we are running \code{awk} on.
##' @param BEGIN A block of code to include as though it were within the \code{BEGIN} block.
##' @param END A block of code to include as though it were within the \code{END} block.
##' @param vars A named list, whereby variables are assigned so that \code{name=value}.
##' @param fs The field separator (passed to \code{-F}).
##' @param out The location to output the result of the computation. If this
##' is \code{TRUE}, we intern the process and bring the results back into the \R 
##' session. Otherwise, it should be a string specifying the output path for a
##' file.
##' @param verbose Output the generated \code{awk} code?
##' @export
##' @examples \dontrun{
##' dat <- data.frame( 
##'   x=1:10, 
##'   y=letters[1:10], 
##'   z=LETTERS[1:10] 
##' )
##' 
##' tempfile <- tempfile()
##' 
##' write.table(dat, 
##'   file=tempfile, 
##'   row.names=FALSE, 
##'   col.names=FALSE, 
##'   quote=FALSE
##' )
##' 
##' x <- awk("print $1", tempfile) 
##' ## note that it is read in as type 'character'
##' print( cbind( x, dat$x ) )
##' }
awk <- function( code, file, BEGIN=NULL, END=NULL, vars=NULL, fs=NULL, out=TRUE, verbose=FALSE ) {
  
  code_statement <- paste("{", code, "}")
  
  if( !is.null(BEGIN) ) {
    BEGIN_statement <- paste( sep="", "BEGIN {", BEGIN, "}")
  } else {
    BEGIN_statement <- NULL
  }
  
  if( !is.null(END) ) {
    END_statement <- paste( sep="", "END {", END, "}")
  } else {
    END_statement <- NULL
  }
  
  if( !is.null(vars) ) {
    vars_statement <- paste("-v", paste( names(vars), vars, sep="=", collapse=" "))
  } else {
    vars_statement <- NULL
  }
  
  if( !is.null(fs) ) {
    if( length(fs) != 1 ) {
      stop("'fs' must be of length 1")
    }
    fs_statement <- paste( sep="", "-F\"", fs, "\"")
  } else {
    fs_statement <- NULL
  }
  
  if( isTRUE(out) ) {
    intern <- TRUE
  } else {
    intern <- FALSE
    out <- suppressWarnings( normalizePath( as.character(out) ) )
  }
  
  awk_call <- paste( sep="",
    .Kmisc$awk,
    " ",
    if( !is.null(vars_statement) ) paste(sep="", vars_statement, " "),
    if( !is.null(fs_statement) ) paste(sep="", fs_statement, " "),
    "'",
    BEGIN_statement, " ",
    code_statement, " ",
    END_statement, "' ",
    file,
    if( !isTRUE(out) ) paste(" >", out)
  )
  
  if( verbose ) print( awk_call )
  
  system( awk_call, intern=intern )
  
}

##' Set awk
##' 
##' Use this function to set the string by which \code{awk} is called; e.g. if
##' you're using GNU awk (gawk), mawk, and so on.
##' 
##' @param awk String denoting the appropriate call for your flavour of \code{awk}.
##' @export
awk.set <- function(awk) {
  .Kmisc$awk <- awk
}

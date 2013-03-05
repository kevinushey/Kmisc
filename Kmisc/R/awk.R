#' A Simple Front-end to Awk
#' 
#' This function provides a simple front-end to \code{awk}.
#' 
#' @param code The \code{awk} code you want to put in the main execution block.
#' @param BEGIN A block of code to run as though it were within the \code{BEGIN} block.
#' @param END A block of code to run as though it were within the \code{END} block.
#' @param vars A named list, whereby variables are assigned so that \code{name=value}.
#' @param fs The field separator (passed to \code{-F}).
#' @param file The file we are running \code{awk} on.
#' @param out The location to output the result of the computation. If this
#' is \code{TRUE}, we intern the process and bring the results back into the \R 
#' session. Otherwise, it should be a string specifying the output path for a
#' file.
#' @param verbose Output the generated \code{awk} code?
#' @export
awk <- function( code, BEGIN=NULL, END=NULL, vars=NULL, fs=NULL, file, out=TRUE, verbose=FALSE ) {
  
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
      stop("'fs' must be of lengh 1")
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
    if( !file.exists(out) ) {
      stop("No file exists at 'out'")
    }
  }
  
  awk_call <- paste( sep="",
                     "awk ",
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
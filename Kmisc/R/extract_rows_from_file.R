#' Extract Rows from File
#' 
#' This function reads through a delimited file on disk, determines if the
#' entry at the specified column is in a character vector of items, and writes
#' that line to file (or to \R) if it is.
#' 
#' @param file The input file to extract rows from.
#' @param out The location to output the file. If this is \code{NULL}, we
#' redirect output back into the \R session.
#' @param column The column to check, indexed from 1.
#' @param sep The delimiter used in \code{file}. Must be a single character.
#' @param keep A character vector containing all items that we want to check
#' and keep within the \code{column}th column of each row.
#' @seealso \code{\link{split_file}}
#' @export
extract_rows_from_file <- function(
  file,
  out=NULL,
  column,
  sep,
  keep
  ) {
  
  if( length(file) > 1 ) {
    stop("'file' must be a character of length one")
  }
  
  if( !file.exists(file) ) {
    stop("No file exists at file location: '", normalizePath(file, mustWork=FALSE), "'")
  }
  file <- normalizePath(file)
  if( !is.null(out) ) out <- suppressWarnings( normalizePath(out) )
  
  if( missing(column) ) {
    stop("You must specify an integer for 'column'")
  }
  
  if( column < 1 ) {
    stop("'column' must be >= 1. (Note that column is 1-indexed)")
  }
  
  if( length(sep) > 1 | nchar(sep) > 1 ) {
    stop("'sep' must be a single character")
  }
  
  if( !is.null(out) && length(out) > 1 ) {
    stop("'out' must be a character of length one")
  }
  
  if( !is.null(out) ) {
    invisible( .Call( "Kmisc_extract_rows_from_file_to_file",
                      as.character(file),
                      as.character(out),
                      as.character(sep),
                      as.character(keep),
                      as.integer(column)
    ) )
  } else {
    return( .Call( "Kmisc_extract_rows_from_file",
                      as.character(file),
                      as.character(sep),
                      as.character(keep),
                      as.integer(column)
                      ) )
  }
  
}
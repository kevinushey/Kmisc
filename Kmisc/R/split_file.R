#' Split a File by Unique Entries in a Column with Awk
#' 
#' This script calls \code{awk} in order to split a file according to
#' unique entries in a column. The name of the entry being split over is
#' appended to the file name (before the file extension).
#' 
#' This function requires the command line tools \code{awk}, \code{uniq}
#' and \code{sort} to be available.
#' 
#' @param file The location of the file we are splitting.
#' @param column The column (by index) to split over.
#' @param prepend A string to prepend to the output file name.
#' @export
split_file <- function( file, column, prepend="" ) {
  
  if( length( prepend ) > 1 ) {
    warning("prepend is of length > 1; only first element will be used")
    prepend <- prepend[1]
  }
  prepend <- as.character(prepend)
  
  if( !file.exists(file) ) {
    stop("No file available at", file)
  }
  
  cat("Getting the unique column names...\n")
  col_names <- (function() {
    
    awk_print <- paste( sep="", "print $", column )
    
    awk_call <- paste( sep="",
                       "awk '{", awk_print, "}' ", file, " | sort | uniq"
    )
    
    return( system( intern=TRUE, awk_call  ) )
  })()
  
  for( name in col_names ) {
    cat("Writing out file for column", column, "equal to", name, "...\n")
    awk_if <- paste( sep="", "if( $", column, " == ", name, ") " )
    awk_call <- paste( sep="", "awk '{ ", awk_if, " print $0 }' ", file )
    outFile <- paste( sep="", strip_extension(file), "_", prepend, name, ".txt" )
    system( paste( sep="", awk_call, " > ", outFile ) )
  }
  
  cat("Done!\n")
}
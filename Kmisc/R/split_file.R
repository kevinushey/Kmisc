#' Split a File by Unique Entries in a Column with Awk
#' 
#' This script calls \code{awk} in order to split a file according to
#' unique entries in a column. The name of the entry being split over is
#' appended to the file name (before the file extension).
#' 
#' This function requires the command line tools \code{awk}, \code{uniq}
#' and \code{sort} to be available. If you're running Windows, please either
#' install cygwin, or have Rtools installed and available on your PATH.
#' 
#' @param file The location of the file we are splitting.
#' @param column The column (by index) to split over.
#' @param outDir The directory to output the files. We default to the 
#' sub-directory \code{split} of the current working directory.
#' @param prepend A string to prepend to the output file names; typically an 
#' identifier for what the column is being split over.
#' @param dots The number of dots used in making up the file extension.
#' If there are no dots in the file name, this argument is ignored.
#' @export
split_file <- function( file, 
                        column, 
                        outDir=file.path( getwd(), "split" ), 
                        prepend="", 
                        dots=1 ) {
  
  if( !file.exists(file) ) {
    stop("No file available at", file)
  }
  
  if( missing(column) ) {
    stop("You must specify a column index to split over")
  }
  
  dir.create( outDir, showWarnings=FALSE)
  
  if( length( fgrep(".", basename(file)) ) > 0 ) {
    file_split <- unlist( strsplit( file, ".", fixed=TRUE ) )
    file_ext <- paste( sep=".", file_split[ (length(file_split)-dots+1):length(file_split) ] )
    file_name <- file_split[ 1:(length(file_split)-dots) ]
  } else {
    file_name <- basename(file)
    file_ext <- ""
  }
  
  if( length( prepend ) > 1 ) {
    warning("prepend is of length > 1; only first element will be used")
    prepend <- prepend[1]
  }
  prepend <- as.character(prepend)
  
  cat("Getting the unique column entries...\n")
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
    outFile <- paste( sep="", file_name, "_", prepend, name, ".", file_ext )
    system( paste( sep="", awk_call, " > ", file.path( outDir, outFile ) ) )
  }
  
  cat("Done!\n")
}
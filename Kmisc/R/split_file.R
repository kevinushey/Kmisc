#' Split a File by Unique Entries in a Column with Awk
#' 
#' This script calls \code{awk} in order to split a file according to
#' unique entries in a column. The name of the entry being split over is
#' appended to the file name (before the file extension).
#' 
#' @param file The location of the file we are splitting.
#' @param column The column (by index) to split over.
#' @param sep The file separator. Must be a single character.
#' @param outDir The directory to output the files. We default to the 
#' sub-directory \code{split} of the current working directory.
#' @param prepend A string to prepend to the output file names; typically an 
#' identifier for what the column is being split over.
#' @param dots The number of dots used in making up the file extension.
#' If there are no dots in the file name, this argument is ignored.
#' @export
split_file <- function( file, 
                        column,
                        sep="\t",
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
  
  ## read the file
  if( length( grep( "gz$", file ) ) > 0 ) {
    conn <- gzfile( file )
  } else {
    conn <- file( file, "r" )
  }
  on.exit( close(conn), add=TRUE )
  
  ## read the file line by line, and process it
  
  ## seen_cols: the column entries that we have seen so far in the file.
  
  ## if we have not yet seen a particular column entry, 
  ## we open a new file connection for that column,
  ## and write to it.
  
  ## if we have seen a particular column entry,
  ## we write the line to that particular file connection
  seen_cols <- character()
  files <- list()
  on.exit( lapply( files, close ), add=TRUE )
  while( length( line <- readLines( conn, 1 ) ) ) {
    line_split <- unlist( strsplit( line, sep, fixed=TRUE ) )
    if( !(line_split[column] %in% seen_cols) ) {
      cat("Encountered new column:", line_split[column], "\n")
      seen_cols <- append( seen_cols, line_split[column] )
      files[[ line_split[column] ]] <- 
        file( open="w", 
              file.path( outDir, 
                         paste( sep="", file_name, "_", prepend, line_split[column], ".", file_ext )
              )
        )
    }
    write( line, files[[ line_split[column] ]] )
  }
  
}
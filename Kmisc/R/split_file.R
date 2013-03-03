#' Split a File by Unique Entries in a Column
#' 
#' This script splits a delimited file by
#' unique entries in a selected column. The name of the entry being split over is
#' appended to the file name (before the file extension).
#' 
#' This function should help users out in the unfortunate case that the data
#' they are attempted to read is too large to fit into RAM. By splitting the
#' file into multiple, smaller files, we hope that each file, post-splitting,
#' is now small enough to fit into RAM.
#' 
#' @param file The location of the file we are splitting.
#' @param column The column (by index) to split over.
#' @param sep The file separator. Must be a single character.
#' @param outDir The directory to output the files.
#' @param prepend A string to prepend to the output file names; typically an 
#' identifier for what the column is being split over.
#' @param dots The number of dots used in making up the file extension.
#' If there are no dots in the file name, this argument is ignored.
#' @param skip Integer; number of rows to skip (e.g. to avoid a header).
#' @param verbose Be chatty?
#' @export
split_file <- function( file, 
                        column,
                        sep="\t",
                        outDir=file.path( dirname(file), "split"), 
                        prepend="", 
                        dots=1,
                        skip=0,
                        verbose=TRUE) {
  
  file <- normalizePath(file)
  if( !file.exists(file) ) {
    stop("No file available at", file)
  }
  
  if( !file.exists(outDir) ) {
    cat("A new directory was created for the post-split files at:\n\t", outDir)
    dir.create( outDir, showWarnings=FALSE)
  }
  outDir <- normalizePath(outDir)
  
  if( missing(column) ) {
    stop("You must specify a column index to split over")
  }
  
  if( length( grep(".", basename(file), fixed=TRUE) ) > 0 ) {
    file_split <- unlist( strsplit( file, ".", fixed=TRUE ) )
    file_ext <- paste( sep="", collapse=".", ".", file_split[ (length(file_split)-dots+1):length(file_split) ] )
    file_name <- strip_extension( basename(file), lvl=dots )
  } else {
    file_name <- strip_extension( basename(file), lvl=dots )
    file_ext <- ""
  }
  
  if( length( prepend ) > 1 ) {
    warning("prepend is of length > 1; only first element will be used")
    prepend <- prepend[1]
  }
  
  prepend <- as.character(prepend)
  
  invisible( .Call( "Kmisc_split_file",
         as.character(file),
         as.character(outDir),
         as.character(file_name),
         .Platform$file.sep,
         as.character(sep),
         as.character(file_ext),
         as.integer(column),
         as.integer(skip),
         as.logical(verbose),
         PACKAGE="Kmisc" ) )
  
}
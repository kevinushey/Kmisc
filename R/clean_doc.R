##' Clean Documentation in Current Package
##' 
##' This function removes all the .Rd documentation files present in
##' \code{<dir>/man}. This function is handy if you've 'polluted' your
##' \code{man} directory in prototyping different functions -- assuming that
##' you're documenting your code with eg. \code{roxygen}.
##' 
##' @param dir the project directory.
##' @param ask boolean. ask before clearing directory?
##' @export
clean_doc <- function(dir=getwd(), ask=TRUE) {
  
  man_dir <- file.path( dir, "man" )
  files <- list.files( man_dir, full.names=TRUE )
  if( length(files) == 0 ) {
    stop("No files in ", dir)
  }
  if( ask ) {
    cat("Are you sure you want to delete all the files in:\n\t", man_dir, "? (y/n):\n")
    if( re_exists( "^y", scan(what=character(), n=1, quiet=TRUE) ) ) {
      for( file in files ) file.remove(file)
      cat("Documentation files deleted.\n")
    } else {
      cat("No files deleted.\n")
      return( invisible(NULL) )
    }
  } else {
    for( file in files ) file.remove(file)
  }
  
}

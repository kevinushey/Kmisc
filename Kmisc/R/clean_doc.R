#' Clean Documentation in Current Package
#' 
#' This function removes all the .Rd documentation files present in
#' \code{<dir>/man}. This function is handy if you've 'polluted' your
#' manual directory in prototyping different functions - assuming that
#' you're documenting your code with eg. \code{roxygen}.
#' 
#' @param dir the project directory.
#' @export
clean_doc <- function(dir=getwd()) {
  files <- list.files( paste( sep="", dir, "/man" ), full.names=TRUE )
  for( file in files ) file.remove(file)
}

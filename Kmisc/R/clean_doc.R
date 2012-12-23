#' Clean Documentation in Current Package
#' 
#' This function removes all the .Rd documentation files present in
#' \code{<dir>/man}. This function is handy if you've 'polluted' your
#' manual directory in prototyping different functions.
#' 
#' @param dir the project directory.
clean_doc <- function(dir=getwd()) {
  files <- list.files( paste0( dir, "/man" ), full.names=TRUE)
  for( file in files ) file.remove(file)
}
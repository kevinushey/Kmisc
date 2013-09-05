##' Update Date in DESCRIPTION File
##' 
##' This function for package authors updates the time in the \code{DESCRIPTION}
##' file to the current date, as discovered through \code{Sys.Date()}.
##' @param file The path to the \code{DESCRIPTION} file.
update_date <- function(file="DESCRIPTION") {
  
  DESCRIPTION <- tryCatch( 
    scan( what=character(), sep="\n", quiet=TRUE, file ),
    error=function(e) {
      stop("DESCRIPTION file not found.")
    } )
  
  DESCRIPTION <- gsub("^Date:.*",
                      paste( sep="", "Date: ", Sys.Date() ),
                      DESCRIPTION, 
                      perl=TRUE )
  
  cat( DESCRIPTION, file=file, sep="\n" )
  message("DESCRIPTION date successfully updated.")
  
}

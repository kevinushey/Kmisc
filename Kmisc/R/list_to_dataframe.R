#' Convert list to data.frame
#' 
#' This function converts a list to a data frame, assuming that each
#' element of the list is of equal length.
#' 
#' @param list A list.
#' @export
list_to_dataframe <- function(list) {
  out <- .Call("list_to_dataframe", list, PACKAGE="Kmisc")
  if( is.null( names(out) ) ) {
    names(out) <- paste( "V", 1:length(out), sep="" )
  }
  return( out )
}

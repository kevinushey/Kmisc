#' Convert list to data.frame
#' 
#' This function converts a list to a data frame, assuming that each
#' element of the list is of equal length.
#' 
#' @param list A list.
#' @param in_place Boolean. If \code{TRUE}, we convert the list in place,
#' so that the \code{list} itself is transformed into a \code{data.frame},
#' sans copying.
#' @export
list_to_dataframe <- function(list, in_place=FALSE) {
  
  if( is.na(in_place) ) {
    stop("'in_place' cannot be NA")
  }
  
  out <- .Call("list_to_dataframe", list, in_place, PACKAGE="Kmisc")
  if( is.null( names(out) ) ) {
    names(out) <- paste( "V", 1:length(out), sep="" )
  }
  return( out )
}

#' @rdname list_to_dataframe
#' @export
list_to_df <- list_to_dataframe

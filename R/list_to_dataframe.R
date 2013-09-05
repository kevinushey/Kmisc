##' Convert list to data.frame
##' 
##' This function converts a list to a data frame, assuming that each
##' element of the list is of equal length.
##' 
##' @param list A list.
##' @param inplace Boolean. If \code{TRUE}, we convert the list in place,
##' so that the \code{list} itself is transformed into a \code{data.frame},
##' sans copying.
##' @export
list_to_dataframe <- function(list, inplace=FALSE) {
  return( .Call("list_to_dataframe", list, inplace, PACKAGE="Kmisc") )
}

##' @rdname list_to_dataframe
##' @export
list_to_df <- list_to_dataframe

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
list2df <- function(list, inplace=FALSE) {
  return( .Call(Clist2df, list, inplace) )
}

##' Convert list to a matrix
##' 
##' This function converts a \code{list} to a \code{matrix}, assuming that each
##' element of the list is of equal length.
##' 
##' @param list A list.
##' @export
list2mat <- function(list) {
  return( .Call(Clist2mat, list) )
}

##' Convert matrix to a list
##' 
##' This function converts a \code{matrix} to a \code{list}.
##' 
##' @param matrix A \code{matrix}.
##' @export
mat2list <- function(matrix) {
  return( .Call(Cmat2list, matrix) )
}

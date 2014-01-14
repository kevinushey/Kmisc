##' Convert a list to a matrix
##' 
##' This function converts a \code{list} to a \code{matrix}, assuming that each
##' element of the list is of equal length.
##' 
##' @param list A list.
##' @export
list2mat <- function(list) {
  return( .Call(Clist2mat, list) )
}

##' Convert a data.frame to a matrix
##' 
##' This function converts a \code{data.frame} to a \code{matrix}.
##' 
##' @param df a \code{data.frame}.
##' @export
list2mat <- function(df) {
  return( .Call(Clist2mat, df) )
}

##' Convert a matrix to a list
##' 
##' This function converts a \code{matrix} to a \code{list}.
##' 
##' @param matrix A \code{matrix}.
##' @export
mat2list <- function(matrix) {
  return( .Call(Cmat2list, matrix) )
}

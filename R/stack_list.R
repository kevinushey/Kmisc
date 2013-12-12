##' Stack a List of DataFrame-like Objects
##' 
##' Function for stacking a list, where each component of the list
##' is a \code{data.frame} containing potentially differing 
##' number of rows, but the same number of columns, and with all columns
##' of equivalent class.
##' 
##' When stacking \code{data.frames} with row names, they 
##' are passed into a column called \code{row_names} to protect from 
##' problems with non-unique row names, and also to avoid appending 
##' numbers onto these row names as well.
##' 
##' Note that information on factors is lost by default; they will be
##' converted either to characters or their internal integer representations.
##' 
##' @param list a list of data frames, or a list of lists with each element of
##' the same length.
##' @param name a name to assign to the column of row names generated.
##' @param make_row_names boolean. add a column built from the row names? Defaults
##' to \code{TRUE} only if the row names are of type \code{character}.
##' @param keep_list_index boolean; if \code{TRUE} we include a vector that indicates
##' which list a particular entry came from.
##' @param index_name a name to assign to the column of indices.
##' @param coerce_factors boolean; if \code{TRUE}, we convert factors to their
##' character representation; otherwise, we take the internal integer representation.
##' @export
##' @examples
##' x <- data.frame( x=c(1, 2, 3), y=letters[1:3], z=rnorm(3), stringsAsFactors=FALSE )
##' rownames(x) <- c("apple", "banana", "cherry")
##' y <- data.frame( x=c(4, 5, 6), y=LETTERS[1:3], z=runif(3), stringsAsFactors=FALSE )
##' rownames(y) <- c("date", "eggplant", "fig")
##' tmp1 <- stack_list( list(x, y) )
##' tmp2 <- do.call( rbind, list(x, y) )
##' rownames(tmp2) <- 1:nrow(tmp2)
##' all.equal( tmp1[,1:3], tmp2 )
stack_list <- function( list, 
                        name="row_names",
                        make_row_names=typeof( attr( list[[1]], "row.names" ) )=="character",
                        keep_list_index=TRUE,
                        index_name="list_index",
                        coerce_factors=TRUE
                        ) {
  
  len <- length( list[[1]] )
  if( inherits( list[[1]], "data.frame" ) ) {
    
    if( coerce_factors ) {
      list <- factor_to_char(list)
    }
    
    classes <- sapply( list[[1]], class )
    if( any( classes == "factor" ) ) {
      warning("factors will be coerced to integers")
    }
    
    out <- .Call( CKmisc_stack_list_df, 
                  list, 
                  classes, 
                  len, 
                  make_row_names, 
                  name, 
                  keep_list_index, 
                  index_name
    )
    
    attr(out, "row.names") <- seq_len( length(out[[1]]) )
    return( out )
  } else {
    return( do.call(rbind, list) )
  }
}

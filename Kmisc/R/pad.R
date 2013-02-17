#' Pad an Object with NAs
#' 
#' This function pads an \R object (list, data.frame, matrix, atomic vector)
#' with \code{NA}s. For matrices, lists and data.frames, this occurs by extending
#' each (column) vector in the object.
#' @param x An \R object (list, data.frame, matrix, atomic vector).
#' @param n The final length of each object.
#' @export
pad <- function(x, n) {
  
  if( class(x) == "data.frame" ) {
    
    nrow <- nrow(x)
    attr(x, "row.names") <- 1:n
    for( i in 1:ncol(x) ) {
      x[[i]] <- c( x[[i]], rep(NA, times=n-nrow) )
    }
    return(x)
    
  } else if( class(x) == "list" ) {
    
    max_len <- max( sapply( x, length ) )
    return( sapply( x, function(xx) {
      return( c( xx, rep(NA, times=n-max_len)))
    }) )
    
  } else if( class(x) == "matrix" ) {
    
    return( rbind( x, matrix(NA, nrow=n-nrow(x), ncol=ncol(x)) ) )
    
  } else {
    
    return( c( x, rep(NA, n-length(x)) ) )
    
  }
  
}

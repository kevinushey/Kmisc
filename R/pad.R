##' Pad an Object with NAs
##' 
##' This function pads an \R object (list, data.frame, matrix, atomic vector)
##' with \code{NA}s. For matrices, lists and data.frames, this occurs by extending
##' each (column) vector in the object.
##' @param x An \R object (list, data.frame, matrix, atomic vector).
##' @param n The final length of each object.
##' @export
pad <- function(x, n) {
  
  if (is.data.frame(x)) {
    
    nrow <- nrow(x)
    attr(x, "row.names") <- 1:n
    for( i in 1:ncol(x) ) {
      x[[i]] <- c( x[[i]], rep(NA, times=n-nrow) )
    }
    return(x)
    
  } else if (is.list(x)) {
    if (missing(n)) {
      max_len <- max( sapply( x, length ) )
      return( lapply(x, function(xx) {
        return( c(xx, rep(NA, times=max_len-length(xx))) )
      }))
    } else {
      return( lapply(x, function(xx) {
        if (n > length(xx)) {
          return( c(xx, rep(NA, times=n-length(xx))) )
        } else {
          return(xx)
        }
      }))
    }
  } else if (is.matrix(x)) {
    
    return( rbind( x, matrix(NA, nrow=n-nrow(x), ncol=ncol(x)) ) )
    
  } else {
    
    return( c( x, rep(NA, n-length(x)) ) )
    
  }
  
}

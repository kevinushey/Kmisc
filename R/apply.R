##' Apply Wrappers
##' 
##' These are thin but clearer wrappers to 
##' \code{apply(x, 1, FUN, ...)} (row apply) and
##' \code{apply(x, 2, FUN, ...)} (column apply).
##' Intended for use with 2D \R \code{matrix}s.
##' We do a bit more work to ensure row names,
##' column names are passed along if appropriate.
##' 
##' See \code{\link{apply}} for more info.
##' 
##' @param X A matrix, or a 2D array.
##' @param FUN The function to be applied.
##' @param ... Optional arguments to \code{FUN}.
##' @param drop Boolean. If \code{TRUE}, we 'drop' dimensions so that results
##'  of dimension \code{n x 1} or \code{1 x n} are coerced to vectors.
##' @rdname apply
##' @export
rowApply <- function(X, FUN, ..., drop=TRUE) {
  output <- apply(X, 1, FUN, ...)
  if (is.matrix(output)) {
    output <- t(output)
    rownames(output) <- rownames(X)
  } else {
    if (drop) {
      output <- c(output)
      names(output) <- rownames(X)
    } else {
      if (!is.matrix(output)) {
        output <- matrix(output, nrow=nrow(X))
      }
      rownames(output) <- rownames(X)
    }
  }
  return(output)
}

##' @rdname apply
##' @export
colApply <- function(X, FUN, ..., drop=TRUE) {
  if (drop) {
    output <- apply(X, 2, FUN, ...)
    if (!is.matrix(output))
      names(output) <- colnames(X)
    return(output)
  } else {
    output <-  matrix( ncol=ncol(X),
      apply(X, 2, FUN, ...)
    )
    colnames(output) <- colnames(X)
    return(output)
  }
}

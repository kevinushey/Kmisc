##' Print a Tree Representation of an Object of Nested Lists
##' 
##' This function returns output similar to that of the command line
##' tool \code{tree}, except rather than directory/file structure,
##' we simply print the names of lists.
##' 
##' @param x A (named) list.
##' @export
##' @seealso http://stackoverflow.com/questions/18122548/display-names-of-column-of-recursive-list-as-tree
tree <- function(x) {
  if (is.list(x)) {
    return( .nametree(x) )
  } else {
    stop("'x' must be a list")
  }
}

.nametree <- function(X, prefix1='', prefix2='', prefix3='', prefix4='') {
  
  if( is.list(X) ) {
    for( i in seq_along(X) ) { 
      nm <- names(X)[i]
      if (is.null(nm)) {
        nm <- "."
      }
      cat( if(i<length(X)) prefix1 else prefix3, nm, "\n", sep="" )
      prefix <- if( i<length(X) ) prefix2 else prefix4
      .nametree(
        X[[i]], 
        paste0(prefix, "|__ "),
        paste0(prefix, "|   "),
        paste0(prefix, "\\__ "),
        paste0(prefix, "    ")
      )
    }
  }
}

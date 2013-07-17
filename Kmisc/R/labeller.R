#' ggplot2 labeller
#' 
#' This function works as a labelling mapper for ggplot2, typically used
#' in \code{facet_grid}. All arguments must be named. Items are mapped as
#' \code{name => value}, where \code{name} represents the original 
#' levels of the factor used for facetting.
#' 
#' @param ... A set of named arguments.
#' @param .parse boolean; if \code{TRUE} we \code{parse} the
#' @export
#' @examples
#' if( require(ggplot2) ) {
#'   df <- data.frame(x=1:100, y=rnorm(100), grp=rep( c("tau+", "tau-"), each=50 ))
#'   f <- labeller( )
#'   ggplot(df, aes(x=x, y=y)) + 
#'     geom_point() + 
#'     facet_grid(". ~ grp", labeller=labeller( `tau+`='tau["+"]', `tau-`='tau["-"]'))
#' }
labeller <- function(..., .parse=TRUE) {
  
  list <- list(...)
  
  return( function(variable, value) {
    
    n <- names(list)
    v <- unlist(list)
    
    if( !all( n %in% value ) || !all( value %in% n) ) {
      warning("some arguments passed to 'labeller' are not levels of the facetting variable")
    }
    
    if( length(n[ n != '']) != length(v) ) {
      stop("mismatch in number of names and number of args; did you pass unnamed arguments to 'labeller'?")
    }
    
    value <- Kmisc::swap(value, names(list), unlist(list))
    output <- vector("list", length(value))
    for( i in seq_along(output)) {
      output[[i]] <- parse( text=value[[i]] )
    }
    return(output)
    
  })
  
}

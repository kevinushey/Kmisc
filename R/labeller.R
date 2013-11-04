##' ggplot2 labeller
##' 
##' This function works as a labelling mapper for \code{ggplot2}, typically used
##' in \code{facet_grid}. All arguments must be named. Items are mapped as
##' \code{name => value}, where \code{name} represents the original 
##' levels of the factor used for facetting.
##' 
##' @param ... A set of named arguments.
##' @param .parse boolean; if \code{TRUE} we \code{parse} the text as
##'   though it were an expression.
##' @export
##' @examples
##' if (require(ggplot2)) {
##'   
##'   df <- data.frame(
##'     x=1:100, 
##'     y=rnorm(100), 
##'     grp=rep( c("tau+", "tau-"), each=50 ) ## levels are "tau+", "tau-"
##'   )
##'   
##'   f <- labeller(
##'     `tau-` = 'tau["-"]',
##'     `tau+` = 'tau["+"]'
##'   )
##'   
##'   ggplot(df, aes(x=x, y=y)) + 
##'     geom_point() + 
##'     facet_grid(". ~ grp", labeller=f)
##'   
##'   df$grp2 <- factor(rep( c("beta+", "beta-"), each=50 ))
##'   
##'   f <- labeller(
##'     `tau-` = 'tau["-"]',
##'     `tau+` = 'tau["+"]',
##'     `beta+` = 'beta["+"]',
##'     `beta-` = 'beta["-"]'
##'   )
##'   
##'   ggplot(df, aes(x=x, y=y)) +
##'     geom_point() +
##'     facet_grid("grp ~ grp2", labeller=f)
##' }
labeller <- function(..., .parse=TRUE) {
  
  list <- list(...)
  
  return( function(variable, value) {
    
    n <- names(list)
    v <- unlist(list)
    
    if( length(n[ n != '']) != length(v) ) {
      stop("mismatch in number of names and number of args; did you pass unnamed arguments to 'labeller'?")
    }
    
    ## swap
    
    #value <- Kmisc::swap(value, n, v)
    
    tmp <- v[ match(value, n)]
    tmp[ is.na(tmp) ] <- value[ is.na(tmp) ]
    value <- tmp
    
    output <- vector("list", length(value))
    for( i in seq_along(output)) {
      output[[i]] <- if (.parse) parse( text=value[[i]] ) else value[[i]]
    }
    return(output)
    
  })
  
}

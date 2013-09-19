##' Customized DataTable for knitr Documents
##' 
##' This function uses a set of nice defaults for use in \code{knitr} 
##' generated HTML documents.
##' 
##' @importFrom rCharts dTable
##' @param x A \code{data.frame}.
##' @param ... Optional arguments. Currently ignored.
# DataTable <- function(x, ...) {
#   dt <- dTable(x, ...)
#   dt$params$width <- 684
#   dt$params$height <- 500
#   return(dt)
# }

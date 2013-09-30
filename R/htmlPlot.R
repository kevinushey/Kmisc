##' Produces Two Versions of a Plot, For Use in R Markdown Documents
##' 
##' 
htmlPlot <- function(plot) {
  
  if (inherits(plot, "trellis")) {
    p1 <- plot
    p2 <- plot
    p2$main <- paste0(p2$main, " - Large")
    print(p1)
    print(p2)
    return( invisible(NULL) )
  }
  
  if (inherits(plot, "ggplot")) {
    p1 <- plot
    p2 <- plot
    if (is.null(p2$labels$title)) {
      p2$labels$title <- "Large"
    } else {
      p2$labels$title <- paste0(p2$labels$title, " - Large")
    }
    print(p1)
    print(p2)
    return( invisible(NULL) )
  }
  
  ## if we get here, assume we have received a base graphics plot
  stop("Currently only handle lattice and ggplot2 plots")
  
}

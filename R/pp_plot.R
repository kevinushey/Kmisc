##' Construct a Probability-Probability Plot from a Set of P-Values
##' 
##' This function constructs a probability-probability plot as based
##' on a vector of p-values.
##' @param x A vector of p-values; numbers within the range 0 to 1.
##' @param ... Optional arguments passed to \code{\link{xyplot}}.
##' Note that a custom panel function is used for generating the plot
##' and hence you shouldn't try to generate your own panel function.
##' @export
##' @examples
##' pp_plot( runif(100), main="PP-Plot of 100 random uniforms" )
pp_plot <- function(x, ...) {
  
  if( any( x < 0 | x > 1 ) ) {
    stop("p-values must be between 0 and 1")
  }
  
  N <- length(x) - sum(is.na(x))
  
  o = -log10(sort(x,decreasing=F))
  e = -log10( (1:length(o))/length(o) )
  
  c975 <- qbeta(0.975, 1:N, N - 1:N + 1)
  c025 <- qbeta(0.025, 1:N, N - 1:N + 1)
  
  upper_band <- -log10(c025) 
  lower_band <- -log10(c975)
  
  print(
    xyplot( o ~ e,
      
      xlim=c(-0.1, max(e, na.rm=TRUE)+0.1),
      ylim=c(-0.1, max(upper_band, o, na.rm=TRUE)*1.1),
      
      panel = function(x, y) {
        
        panel.grid(h=-1,v=-1)
        
        ## the polygon can be too big to plot, so
        ## we take a subset of the generated points.
        
        # n <- seq(1, length(e), by=min( max(length(e)/10000,1), 100 ))
        n <- unique( round( rev(
          10^(seq(0, log10(length(e)), length.out=1000))
        ) ) )
        
        e <- e[n]
        upper_band <- upper_band[n]
        lower_band <- lower_band[n]
        
        tx <- c( rev(e), e )
        ty <- c( rev(lower_band), upper_band )
        panel.polygon(x=tx,y=ty, col=rgb(0.5,0.5,0.5,0.3), border = NA)
        
        ## plot the points + line of y=x
        panel.xyplot( x, y, pch=20, col="black", ... )
        panel.abline(a=0,b=1,col="red")          
      },
      
      xlab = expression( paste( 
        "Expected ", -log[10](italic(P))
      )),
      ylab = expression( paste(
        "Observed ", -log[10](italic(P))
      )), ...
    ) 
  )
  
}

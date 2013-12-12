##' Custom Lattice Boxplot
##' 
##' This generates a custom lattice boxplot; we super-impose actual plot points
##' for groups with a small number of points, and also restrict plotting of the
##' boxplot for these as well (since they are really rather mis-representative
##' of the distribution when there are so few points.) The downside is that
##' functionality is not implemented for multi-panel plots.
##' 
##' Axis labels are inferred from the \code{form} object passed in when possible.
##' 
##' @importFrom lattice stripplot panel.abline panel.bwplot panel.stripplot
##' @param form a formula object as expected by \code{lattice}'s \code{xyplot}.
##' @param data see \code{\link{xyplot}}.
##' @param xlab see \code{\link{xyplot}}.
##' @param ylab see \code{\link{xyplot}}.
##' @param main see \code{\link{xyplot}}.
##' @param n number of points necessary for a boxplot to be drawn.
##' @param ... additional arguments passed to \code{xyplot} call.
##' @export
##' @examples
##' set.seed(123)
##' dat <- data.frame( y=rnorm(100), x=factor( rbinom(100,size=2,p=0.8) ) )
##' bwplot2( x ~ y , dat)
bwplot2 <- function( form, 
                     data=NULL, 
                     xlab=NULL, 
                     ylab=NULL, 
                     main=NULL, 
                     n=10, 
                     ... ) {
  
  if( re_exists(attr( terms(form), "variable" ), "\\|") ) {
    stop("not implemented for multi-panel plots")
  }
  
  terms <- unlist( strsplit( unlist( strsplit( capture.output( form ), " ~ " ) ), " | " ) )
  terms <- terms[ terms != "|" ]
  
  if( is.null(xlab) ) {
    xlab <- gsub( ".*\\$", "", terms[2] )
  }
  
  if( is.null(ylab) ) {
    ylab <- gsub( ".*\\$", "", terms[1] )
  }
  
  xterm <- gsub( ".*\\$", "", terms[2] )
  yterm <- gsub( ".*\\$", "", terms[1] )
  zterm <- gsub( ".*\\$", "", terms[3] )
  
  if( is.null(data) ) {
    tDat <- data.frame(
      y = eval( form[[2]] ),
      x = eval( form[[3]] )
    )
  } else {
    tDat <- data.frame(
      y = data[[yterm]],
      x = data[[xterm]]
    )
  }
  
  if( !is.na(zterm) ) tDat$z <- data[[zterm]]
  
  tDat <- tDat[ !is.na(tDat$x), ]
  tDat$y <- as.character(tDat$y) 
  tDat$y[ is.na(tDat$y) ] <- " Missing"
  
  myTable <- table( tDat$y )
  tDat$y <- paste( tDat$y, ": n = ", myTable[ match(tDat$y, names(myTable) ) ], sep="" )
  
  if( is.null( main ) ) {
    main <- paste( sep="", collapse="", yterm, " vs. ", xterm )
  }
  
  stripplot( y ~ x, tDat,
             par.settings=list(
               plot.symbol = list( col = "black", fill="orange", pch=21, cex=0.7 ),
               box.rectangle = list( col = "black" ),
               box.umbrella = list( col = "black" )
             ),
             
             panel = function(x, y, ...) {
               
               
               for (i in 1:length( unique(y) )) {
                 panel.abline( h = i, col='grey')
               }
               
               cDat <- data.frame( x=x, y=y )
               tTab <- table( cDat$y )
               cDat$num <- tTab[ match( cDat$y, names(tTab) ) ]
               
               ## plot only the values for which we have >=n points
               bDat <- cDat
               bDat <- bDat[ bDat$num >= n, ]
               panel.bwplot(bDat$x, bDat$y, fill="lavender", pch = "|", ...)
               
               ## plot only the values for which we have <n points
               cDat$x[ cDat$num > n ] <- NA
               
               panel.stripplot(cDat$x, cDat$y, 
                               col = "black", pch = 21, fill = "cadetblue", cex=1, ...)
               
               
             },
             ## sub = "jitter added to give idea of density of observations",
             
             xlab=xlab,
             ylab=ylab,
             main=main,
             
             ...
  )
  
}

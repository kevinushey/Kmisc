#' Make a Manhattan Plot
#' 
#' Generates a manhattan plot (a plot of \code{-log10(p-val)}) for a set of 
#' markers by chromosome and base-pair position.
#' 
#' @param pval A vector of p-values.
#' @param bp A vector of base-pair positions, corresponding to the genomic
#' location for which that p-value is associated (typically, the SNP location).
#' @param chr The chromosomal location associated with the p-value.
#' @param groups A groups vector: used if you want to overlay multiple
#' manhattan plots.
#' @param cutoff optional. By default, a Bonferroni cutoff line is drawn
#' on the plot; if you want to plot a custom cut-off line you can specify
#' the cutoff here.
#' @param xlab The label to use for the x axis.
#' @param ylab The label to use for the y axis.
#' @param ... Optional arguments passed to \code{xyplot}.
#' @export
#' @examples
#' pval <- runif(1E4)
#' bp <- c(1:5E3, 1:5E3)
#' chr <- rep(1:22, length.out=1E4)
#' groups=rep( c("Phenotype 1", "Phenotype 2"), each=5E3 )
#' manhattan_plot( pval, bp, chr, groups )
manhattan_plot <- function( pval,
                            bp,
                            chr,
                            groups=NULL,
                            cutoff=NULL,
                            xlab="Chromosome (base-pair position)",
                            ylab=expression( paste( -log[10]( italic(p) ) ) ),
                            ... ) {
  
  dat <- data.frame(
    P=pval,
    BP=bp,
    CHR=chr
  )
  
  if( !is.null(groups) ) {
    dat$GROUP <- factor_(groups)
  }
  
  dat <- dat[ order(dat$CHR), ]
  reptimes <- counts(dat$CHR)
  maxes <- with( dat, tapply( BP, CHR, max ) )
  maxes.cumsum <- cumsum( as.numeric( maxes ) )
  
  tmp <- c(0, maxes.cumsum)
  diffs <- rep(0, length(maxes.cumsum))
  for( i in 1:(length(tmp)-1) ) {
    diffs[i] <- (tmp[i+1] - tmp[i])/2 + tmp[i]
  }
  
  nCHR <- lu( dat$CHR )
  adj <- rep( c(0, maxes.cumsum[-nCHR]), times=reptimes )
  dat$relBP <- dat$BP + adj
  
  ## assign colors based on groups
  cols <- c(
    "#A6CEE3", 
    "#1F78B4", 
    "#B2DF8A", 
    "#33A02C", 
    "#FB9A99", 
    "#E31A1C", 
    "#FDBF6F", 
    "#FF7F00", 
    "#CAB2D6", 
    "#6A3D9A"
  )
  
  cols <- cols[c(1, 2, 7, 8, 9, 10, 3, 4, 5, 6, 11, 12)]
  
  if( is.null(groups) ) {
    cols <- c("grey50", "grey90")
  } else {
    cols <- c(
      cols[seq(1, length.out=lu(dat$GROUP), by=2)],
      cols[seq(2, length.out=lu(dat$GROUP), by=2)]
    )
  }
  
  dat$COL_GROUP <- paste( dat$CHR %% 2 == 0, dat$GROUP, sep="_" )
  dat$COL <- swap( 
    dat$COL_GROUP, 
    unique(dat$COL_GROUP), 
    cols
  )
  
  ## Generate the labels
  kLabel <- 1:nCHR
  kLabel[ kLabel > 12 & kLabel %% 2 == 1 ] <- ""
  
  ## generate the key
  if( is.null(groups) ) {
    myKey <- list()
  } else {
    myKey <- list(
      text=list(
        as.character( unique(dat$GROUP) )
      ),
      points=list(
        col="black",
        fill=cols[1:lu(dat$GROUP)],
        pch=21
      ),
      points=list(
        col="black",
        fill=cols[(lu(dat$GROUP)+1):(2*lu(dat$GROUP))],
        pch=21
      )
    )
  }
  
  print( 
    with( dat, 
          xyplot( -log10(P) ~ relBP,
                  grid=TRUE,
                  pch=21,
                  col="black",
                  fill=COL,
                  cex = 1,
                  scales = list( 
                    x = list(
                      relation = "same",
                      tck = c(1,0),
                      at = diffs,
                      labels = kLabel
                    ) 
                  ),
                  panel = function(x, y, ...) {
                    panel.xyplot(x, y, ...)
                    panel.abline( h=-log10(0.05/nrow(dat)),
                                  col="red", lty="dashed" )
                    panel.abline(h=0, col="black", lwd=8)
                    
                  },
                  key=myKey,
                  xlim = c( 0, max(dat$relBP) ),
                  origin = 0,
                  xlab=xlab,
                  ylab=ylab,
                  ...
          )
    ) 
  )
  
}

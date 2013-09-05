##' Merge a Data Frame 'into' Another
##' 
##' This function will merge a data frame \code{df2} 'into' a data frame
##' \code{df1}, preserving \code{df1} as much as possible in the merger.
##' Hence I call this a 'preserving' merge, or \code{pMerge}.
##' 
##' @param df1 the data.frame which we are preserving
##' @param df2 the data.frame we are merging into df1
##' @param by character; name of the variable we are merging over
##' @param doCheck boolean; set this if you want to perform more extensive 
##' (but slower) error checking
##' @export
##' @examples
##' df1 <- data.frame( stringsAsFactors=FALSE,
##'   x=1:1000,
##'   y=sample(LETTERS, size=1000, replace=TRUE)
##' )
##' 
##' df2 <- data.frame( stringsAsFactors=FALSE,
##'   x=sample( 1:2000, size=2000, replace=TRUE ),
##'   z=sample( letters, size=2000, replace=TRUE ),
##'   q=sample( LETTERS, size=2000, replace=TRUE )
##' )
##' 
##' dMerged <- pMerge( df1, df2, by="x" )
##' stopifnot( all.equal( df1, dMerged[1:ncol(df1)] ) )
pMerge <- function( df1, 
                    df2, 
                    by=intersect( names(df1), names(df2) ), 
                    doCheck=FALSE ) {
  
  if( by %nin% names(df1) ) {
    stop( paste( collapse="", sep="",
                 by, " not in names(", deparse( substitute( df1 ) ), ")" ) 
          )
  }
  
  if( by %nin% names(df2) ) {
    stop( paste( collapse="", sep="",
                 by, " not in names(", deparse( substitute( df2 ) ), ")" ) 
    )
  }
  
  if( doCheck ) {
    
    if( any( table( df1[[by]] ) > 1 ) ) {
      stop( paste("non-unique entries in", deparse( substitute( df1 ) ),
                  "for merger variable", by) )
    }
    
    if( any( tmp <- df2[[by]] %nin% df1[[by]] ) ) {
      which_tmp <- df2[[by]][tmp]
      warning( paste( 
        "some variables found in df2 but not df1:\n\t", which_tmp 
        ) )
    }
    
  }
  
  df2 <- df2[ df2[[by]] %in% df1[[by]], ]
  df2_processed <- as.data.frame( 
    sapply( names(df2)[ names(df2) %nin% by ], function(x) {
      tapply( df2[[x]], df2[[by]], function(xx) {
          paste(xx, collapse="")
      })
    }), optional=TRUE, stringsAsFactors=FALSE )
  df2_processed[[by]] <- rownames(df2_processed)
  
  merge( df1, df2_processed, all.x=TRUE )
  
}

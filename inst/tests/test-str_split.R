library(Kmisc)
library(testthat)



dat <- do.call(paste, replicate(10, sample(letters, 1E5, TRUE), simplify=FALSE))
## old str_split
str_split2 <- function(x, sep, fixed=FALSE, perl=TRUE, useBytes=FALSE, names=NULL) {
  
  if( fixed ) {
    perl <- FALSE
  }
  
  x <- as.character(x)
  tmp <- strsplit( x, sep, fixed=fixed, perl=perl, useBytes=useBytes )
  if( length( unique( sapply( tmp, length ) ) ) > 1 ) {
    stop("non-equal lengths for each entry of x post-splitting")
  }
     tmp <- unlist(tmp)
     tmp <- as.data.frame( matrix( tmp, ncol=length(tmp)/length(x), byrow=TRUE ),
                           stringsAsFactors=FALSE, optional=TRUE
     )
  #tmp <- .Call( "charlist_transpose_to_df", tmp, PACKAGE="Kmisc" )
  if( !is.null(names) ) {
    names(tmp) <- names
  } else {
    names(tmp) <- paste( "V", 1:ncol(tmp), sep="" )
  }
  
  return(tmp)
  
}

if( require(microbenchmark) )
  microbenchmark( str_split(dat, " ", fixed=TRUE), str_split2(dat, " ", fixed=TRUE), times=10 )

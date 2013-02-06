library( testthat )
library( microbenchmark )

dat <- list( 
  list( 
    list( a="1", b=c("2", "3", "4") ),
    list( c=factor(c("a", "b", "c")) ) ), 
  factor( c(1,2,3) )
)

char_to_factor2 <- function(X, ...) {
  return( rapply( X, how="replace", function(x) {
    if( is.character(x) ) {
      return( factor(x, ...) )
    } else {
      return( x )
    }
  }) )
}

expect_identical( factor_to_char(dat), rapply(dat, as.character, how="list") )
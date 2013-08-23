library( Kmisc )
library( testthat )
library( microbenchmark )

dat <- list( 
  list( 
    list( a="1", b=c("2", "3", "4") ),
    list( c=factor(c("a", "b", "c")) ) ), 
  factor( c(1,2,3) )
)

expect_identical( char_to_factor(dat), rapply(dat, as.factor, how="list") )
expect_identical( factor_to_char(dat), rapply(dat, as.character, how="list") )

dat <- char_to_factor(dat)
expect_identical( char_to_factor(factor_to_char(dat)), dat )
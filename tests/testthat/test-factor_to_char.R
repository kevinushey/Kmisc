library(Kmisc)
library( testthat )

dat <- list( 
  list( 
    list( a=factor(1), b=factor(c(2,3,4)) ), 
    list( c=factor(c("a", "b", "c")) ) ), 
  factor(c(1,2,3)) 
  )

expect_identical( factor_to_char(dat), rapply(dat, as.character, how="list") )

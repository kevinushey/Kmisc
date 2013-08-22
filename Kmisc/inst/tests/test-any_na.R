library(Kmisc)
library(testthat)

x <- rnorm(10)
x[1] <- NA
expect_identical( any( is.na(x) ), any_na(x) )
x <- as.character(x)
expect_identical( any( is.na(x) ), any_na(x) )
x <- as.integer(x)
expect_identical( any( is.na(x) ), any_na(x) )
x <- as.factor(x)
expect_identical( any( is.na(x) ), any_na(x) )
x <- as.logical(x)
expect_identical( any( is.na(x) ), any_na(x) )

library(microbenchmark)
x <- rnorm(1E6)
x[1E4] <- NA
microbenchmark( any_na(x), any( is.na( x ) ) )

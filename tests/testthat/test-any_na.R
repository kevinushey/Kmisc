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

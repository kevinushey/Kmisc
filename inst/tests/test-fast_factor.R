library(Kmisc)
library(testthat)
set.seed(123)

n <- 1E4

## integer
gp <- sample( n, replace=TRUE )
expect_identical( factor(gp), factor_(gp) )

gp[ sample(length(gp), 1E1) ] <- NA
expect_identical( factor(gp), factor_(gp) )

## character
gp <- sample( letters, n, replace=TRUE )
expect_identical( factor(gp), factor_(gp) )

gp[ sample(length(gp), 1E1) ] <- NA
expect_identical( factor(gp), factor_(gp) )

## logical
gp <- sample( c(TRUE, FALSE), n, TRUE )
expect_identical( factor(gp), factor_(gp) )

gp[ sample(length(gp), 1E1) ] <- NA
expect_identical( factor(gp), factor_(gp) )

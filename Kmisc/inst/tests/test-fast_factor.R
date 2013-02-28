library(testthat)
set.seed(123)

n <- 1E4

gp <- sample( n, replace=TRUE )
expect_identical( factor(gp), Kmisc:::factor_(gp) )

gp[ sample(length(gp), 1E1) ] <- NA
expect_identical( factor(gp), Kmisc:::factor_(gp) )

gp <- sample( letters, n, replace=TRUE )
expect_identical( factor(gp), Kmisc:::factor_(gp) )

gp[ sample(length(gp), 1E1) ] <- NA
expect_identical( factor(gp), Kmisc:::factor_(gp) )

gp <- sample( c(TRUE, FALSE), 100, replace=TRUE )
expect_identical( factor(gp), Kmisc:::factor_(gp) )
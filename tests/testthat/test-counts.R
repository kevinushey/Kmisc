library(testthat)
library(Kmisc)

table_ <- function(x) {
  tmp <- table(x, useNA="ifany")
  out <- as.integer(tmp)
  names(out) <- dimnames(tmp)[[1]]
  return(out)
}

set.seed(123)
x <- round( rnorm(1E2), 0 )
expect_identical( counts(x), table_(x) )

x <- as.integer(x)
expect_identical( counts(x), table_(x) )

x <- as.character(x)
expect_identical( counts(x), table_(x) )

x <- as.logical( as.numeric(x) )
expect_identical( counts(x), table_(x) )

x <- round( rnorm(1E4), 0 )
x[ sample(1:length(x), 10) ] <- NA
expect_identical( counts(x), table_(x) )

x <- as.integer(x)
expect_identical( counts(x), table_(x) )

x <- as.character(x)
expect_identical( counts(x), table_(x) )

x <- as.logical( as.numeric(x) )
expect_identical( counts(x), table_(x) )

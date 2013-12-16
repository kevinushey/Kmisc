library(testthat)
library(Kmisc)

table_ <- function(x) {
  tmp <- table(x, useNA="ifany")
  out <- as.integer(tmp)
  names(out) <- dimnames(tmp)[[1]]
  return(out)
}

n <- 1E2

set.seed(123)
x <- round( rnorm(n), 0 )
expect_identical( counts(x), table_(x) )


x <- as.integer(x)
expect_identical( counts(x), table_(x) )


x <- as.character(x)
expect_identical( counts(x), table_(x) )


x <- as.logical( as.numeric(x) )
expect_identical( counts(x), table_(x) )


x <- round( rnorm(n), 0 )
x[ sample(1:length(x), 10) ] <- NA
expect_identical( counts(x), table_(x) )


x <- as.integer(x)
expect_identical( counts(x), table_(x) )


x <- as.character(x)
expect_identical( counts(x), table_(x) )


x <- as.logical( as.numeric(x) )
expect_identical( counts(x), table_(x) )


x <- replicate(10, round( rnorm(n), 0 ), simplify=FALSE)
expect_identical(
  counts(x),
  lapply(x, table_)
)

## test small, large numerics
expect_identical( counts(1E-20), c(table(1E-20)) )
expect_identical( counts(1E20), c(table(1E20)) )
expect_identical( counts(TRUE), c(table(TRUE)) )
expect_identical( counts(FALSE), c(table(FALSE)) )
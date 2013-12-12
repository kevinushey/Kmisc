library(testthat)
library(Kmisc)

table_ <- function(x) {
  tmp <- table(x)
  out <- as.integer(tmp)
  names(out) <- dimnames(tmp)[[1]]
  return(out)
}

set.seed(123)
x <- round( rnorm(1E4), 0 )
y <- as.integer(x)
expect_identical( counts(x), table_(x) )

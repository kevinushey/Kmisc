library(Kmisc)
library(testthat)

n <- 1E2

tmp <- data.frame( stringsAsFactors=FALSE,
  x=sample(letters, n, TRUE),
  y=sample(LETTERS, n, TRUE),
  k=sample(LETTERS, n, TRUE),
  za=rnorm(n),
  zb=rnorm(n),
  zc=rnorm(n)
)
m <- melt_(tmp, id.vars=c('x', 'y', 'k'))

m1 <- unmelt_(m)

test <- function() {
  expect_identical(
    unmelt_(melt_(tmp, id.vars=c('x', 'y', 'k'))),
    tmp
  )
}

test()

tmp <- data.frame( stringsAsFactors=FALSE,
  x=sample(letters, n, TRUE),
  y=sample(LETTERS, n, TRUE),
  k=sample(LETTERS, n, TRUE),
  za=as.integer(rnorm(n)),
  zb=as.integer(rnorm(n)),
  zc=as.integer(rnorm(n))
)

test()

tmp <- data.frame( stringsAsFactors=FALSE,
  x=sample(letters, n, TRUE),
  y=sample(LETTERS, n, TRUE),
  k=sample(LETTERS, n, TRUE),
  za=as.character(rnorm(n)),
  zb=as.character(rnorm(n)),
  zc=as.character(rnorm(n))
)

test()

## check for an error if data malformed
m <- melt_(tmp, id=c('x', 'y', 'k'))
m$x[nrow(m)] <- "wat"
expect_error(unmelt_(m))

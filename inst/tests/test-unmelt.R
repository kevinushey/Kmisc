library(Kmisc)
library(testthat)

n <- 1E5

tmp <- data.frame( stringsAsFactors=FALSE,
  x=sample(letters, n, TRUE),
  y=sample(LETTERS, n, TRUE),
  za=rnorm(n),
  zb=rnorm(n),
  zc=rnorm(n)
)
m <- melt_(tmp, id.vars=c('x', 'y'))

m1 <- unmelt(m)
m2 <- unmelt2(m)

test <- function() {
  expect_identical(
    unmelt(melt_(tmp, id.vars=c('x', 'y'))),
    tmp
  )
}

test()

tmp <- data.frame( stringsAsFactors=FALSE,
  x=sample(letters, n, TRUE),
  y=sample(LETTERS, n, TRUE),
  za=as.integer(rnorm(n)),
  zb=as.integer(rnorm(n)),
  zc=as.integer(rnorm(n))
)

test()

tmp <- data.frame( stringsAsFactors=FALSE,
  x=sample(letters, n, TRUE),
  y=sample(LETTERS, n, TRUE),
  za=as.character(rnorm(n)),
  zb=as.character(rnorm(n)),
  zc=as.character(rnorm(n))
)

test()

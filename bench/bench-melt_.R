library(microbenchmark)
library(Kmisc)
library(data.table)

n <- 1E6
dat <- data.frame( stringsAsFactors=FALSE,
  x=sample(letters, n, TRUE), 
  y=sample(LETTERS, n, TRUE),
  za=rnorm(n), 
  zb=rnorm(n), 
  zc=rnorm(n)
)

dat <- data.table(dat)
microbenchmark( times=50,
  tmp1 <- melt_(dat, id.vars=c('x', 'y')),
  tmp2 <- data.table:::melt.data.table(dat, id.var=c('x', 'y'))
)

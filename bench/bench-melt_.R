library(microbenchmark)
library(Kmisc)
library(data.table)

n <- 1E5
dat <- data.frame( stringsAsFactors=FALSE,
  x=sample(letters, n, TRUE), 
  y=sample(LETTERS, n, TRUE),
  za=rnorm(n), 
  zb=rnorm(n), 
  zc=rnorm(n)
)

dat <- data.table(dat)
microbenchmark( times=5,
  reshape2:::melt.data.frame(dat, id.vars=c('x', 'y')),
  melt_(dat, id.vars=c('x', 'y')),
  data.table:::melt.data.table(dat, id.var=c('x', 'y'))
)

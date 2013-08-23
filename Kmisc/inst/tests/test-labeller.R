library(testthat)
library(ggplot2)

df <- data.frame( x=1:100, y=rnorm(100), grp=rep( letters[1:2], each=50 ))
p <- ggplot(df, aes(x=x, y=y)) + geom_point()

p + facet_grid( ". ~ grp", labeller=labeller(a="alpha", b="beta"))

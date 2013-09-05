library(testthat)

x <- list( 1:10, factor(1:10), list(1:5, "a", "b"), "z" )
gctorture(TRUE)

for(i in 1:10) {
  print( factor_to_char(x) )
}
gctorture(FALSE)

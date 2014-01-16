library(testthat)

df <- data.frame(x=1, y=2, z=3)
df2 <- list2df( df2list(df) )
expect_identical(
  df,
  df2
)

expect_warning( list2df(df) )

list <- list(x=1, y=2, z=3)
list2 <- df2list( list2df( list ) )
expect_identical(
  list,
  list2
)

expect_error( df2list(list) )

library(testthat)
library(microbenchmark)

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

big_list <- replicate(10, rnorm(1E5), simplify=FALSE)
microbenchmark( times=5,
  as.data.frame(big_list),
  list2df(big_list)
)

big_df <- list2df(big_list)
microbenchmark( times=5,
  as.list(big_df),
  df2list(big_df)
)

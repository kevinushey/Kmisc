context("coerce")

## factor2char
dat <- list( 
  list( 
    list( a=factor(1), b=factor(c(2,3,4)) ), 
    list( c=factor(c("a", "b", "c")) ) ), 
  factor(c(1,2,3)) 
)

expect_identical( factor_to_char(dat), rapply(dat, as.character, how="list") )

## df2list
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

## list2df
l1 <- list( a=1:100, b=sample( c(TRUE, FALSE), 100, TRUE ) )
l1$c <- factor(l1$a)
l1$d <- sample( letters, 100, TRUE )

list2df(l1)
names(l1) <- NULL

list2df(l1)
expect_error(list2df(l1, inplace=NA))

l <- replicate(10, 1:10, simplify=FALSE)
df <- list2df(l)
m <- list2df(l, inplace=TRUE)

## mat2df
m <- matrix(1:9, nrow=3)
expect_identical(
  unname(as.matrix( mat2df(m) )),
  m
)

m1 <- mat2df(m)
gctorture(TRUE)
m2 <- mat2df(m)
gctorture(FALSE)
expect_identical(m1, m2)

expect_identical(
  mat2df(m), as.data.frame(m)
)

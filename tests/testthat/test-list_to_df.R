library(testthat)

l1 <- list( a=1:100, b=sample( c(TRUE, FALSE), 100, TRUE ) )
l1$c <- factor(l1$a)
l1$d <- sample( letters, 100, TRUE )

list2df(l1)
names(l1) <- NULL

list2df(l1)
expect_error(list2df(l1, inplace=NA))

l <- replicate(1E5, 1:10, simplify=FALSE)
df <- list2df(l)
m <- list2df(l, inplace=TRUE)
m[,1] <- 10

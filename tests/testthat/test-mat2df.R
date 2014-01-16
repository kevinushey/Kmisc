library(Kmisc)

m <- matrix(1:9, nrow=3)
stopifnot( identical(
  unname(as.matrix( mat2df(m) )),
  m
) )

m1 <- mat2df(m)
gctorture(TRUE)
m2 <- mat2df(m)
gctorture(FALSE)
stopifnot( identical( m1, m2 ) )

stopifnot( identical(
  mat2df(m), as.data.frame(m)
) )

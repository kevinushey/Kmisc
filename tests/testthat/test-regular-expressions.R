context("Regular Expressions")

test_that("re_* functions work with various objects", {

  ## Re: GitHub issue #9
  library(data.table)
  dt <- data.table(
    id=letters[1:6],
    gender=rep(c('M','F'),times=3),
    dv1=runif(6),
    dv2=rnorm(6)
  )

  dt_sub <- re_without(dt,"^dv\\d")
  expect_equal(
    as.data.frame(dt_sub),
    re_without(as.data.frame(dt), "^dv\\d")
  )

  dt_sub <- re_extract(dt, "[dr]$")
  expect_equal(
    as.data.frame(dt_sub),
    extract(as.data.frame(dt), id, gender)
  )

  ## Other basic tests
  m <- matrix(1:25, nrow = 5)
  rownames(m) <- letters[1:5]
  expect_identical(
    re_extract_rows(m, "[a-c]"),
    m[c("a", "b", "c"), ]
  )


})

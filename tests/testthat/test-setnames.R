context("setnames")

test_that("setrownames, setcolnames behave as expected for data.frames", {
  
  df <- data.frame(x=1, y=2, z=3)
  setrownames(df, "foo")
  setcolnames(df, c("foo", "bar", "baz"))
  expect_identical( rownames(df), "foo" )
  expect_identical( colnames(df), c("foo", "bar", "baz") )
  expect_error( setrownames(df, c("foo", "bar")) )
  expect_error( setcolnames(df, "bar") )
    
})

test_that("setrownames, setcolnames behave as expected for matrices", {
  
  m <- matrix(1:9, nrow=3)
  setrownames(m, letters[1:3])
  setcolnames(m, LETTERS[1:3])
  expect_identical(rownames(m), letters[1:3])
  expect_identical(colnames(m), LETTERS[1:3])
  expect_error(setrownames(m, "foo"))
  expect_error(setcolnames(m, "bar"))
  
})

context("str_split")

expect_identical(
  str_split(
    c("regular_structure", "in_my", "data_here"),
    sep="_",
    names=c("apple", "banana")
  ),
  data.frame(
    apple=c("regular", "in", "data"),
    banana=c("structure", "my", "here"),
    stringsAsFactors=FALSE
  )
)

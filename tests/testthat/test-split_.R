context("split")

num <- rnorm(10)
int <- 1:10
char <- letters[1:10]
lgcl <- as.logical(int)

for (x1 in list(num, int, char, lgcl)) {
  for (x2 in list(num, int, char, lgcl)) {
    expect_identical(
      unname(split(x1, x2)),
      unname(split_(x1, x2))
    )
  }
}

num[ sample(10, 5) ] <- NA
int[ sample(10, 5) ] <- NA
char[ sample(10, 5) ] <- NA
lgcl[ sample(10, 5) ] <- NA

for (x1 in list(num, int, char, lgcl)) {
  for (x2 in list(num, int, char, lgcl)) {
    expect_identical(
      unname(split(x1, x2)),
      unname(split_(x1, x2, na.last=FALSE))
    )
  }
}

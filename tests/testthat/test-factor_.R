context("fast_factor, factor_")

set.seed(123)

n <- 1E2

## integer
gp <- sample( n, replace=TRUE )
expect_identical( factor(gp), factor_(gp) )

gp[ sample(length(gp), 1E1) ] <- NA
expect_identical( factor(gp), factor_(gp) )

## character
gp <- sample( letters, n, replace=TRUE )
expect_identical( factor(gp), factor_(gp) )

gp[ sample(length(gp), 1E1) ] <- NA
expect_identical( factor(gp), factor_(gp) )

## logical
gp <- sample( c(TRUE, FALSE), n, TRUE )
expect_identical( factor(gp), factor_(gp) )

gp[ sample(length(gp), 1E1) ] <- NA
expect_identical( factor(gp), factor_(gp) )

num <- rnorm(10)
int <- 1:10
char <- letters[1:10]
lgcl <- as.logical(int)

for (x1 in list(num, int, char, lgcl)) {
  expect_identical(
    factor(x1),
    factor_(x1)
  )
}

num[ sample(10, 5) ] <- NA
int[ sample(10, 5) ] <- NA
char[ sample(10, 5) ] <- NA
lgcl[ sample(10, 5) ] <- NA

for (x1 in list(num, int, char, lgcl)) {
  expect_identical(
    factor(x1),
    factor_(x1)
  )
}

# num[ sample(10, 5) ] <- NaN
# expect_identical( factor(num), factor_(num) )

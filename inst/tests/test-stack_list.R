library(Kmisc)
library(testthat)

dat <- vector("list", 10)
for( i in 1:10 ) { 
  dat[[i]] <- data.frame( x=rnorm(10), y=sample(letters, 10), z=sample(LETTERS,10) ) 
}

unrowname <- function(x) {
  rownames(x) <- 1:nrow(x)
  return(x)
}

expect_identical( 
  stack_list(dat)[,1:length(dat[[1]])], 
  factor_to_char( unrowname( do.call( rbind, dat ) ) ) 
)

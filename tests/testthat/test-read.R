library(testthat)
library(Kmisc)
library(microbenchmark)
library(Rcpp)

dat <- as.data.frame( replicate(10, rnorm(1E4), simplify=FALSE) )
tempfile <- tempfile()
write.table(dat, file=tempfile)

tmp1 <- readLines(tempfile)
tmp2 <- readlines(tempfile)
stopifnot( identical(tmp1, tmp2) )

tmp3 <- read(tempfile)
stopifnot( identical( tmp2, unlist(strsplit(tmp3, "\n", fixed=TRUE)) ) )

cppFunction(verbose=TRUE, includes="
#include <Rcpp.h>
#include <fstream>
", code='
CharacterVector read_lines(std::string path) {
  using namespace std;
  ifstream t(path.c_str());
  string tmp;
  vector<string> out;
  while (getline(t, tmp)) {
    out.push_back(tmp);
  }
  return wrap(out);
}
')

rbenchmark::benchmark( replications=5,
  readlines(tempfile),
  readLines(tempfile),
  read_lines(tempfile)
)

unlink(tempfile)

files <- list.files("~", full.names=TRUE, pattern="py$|R$|Rmd$|tex$")
for (i in seq_along(files)) {
  x <- files[i]
  read(x)
  stopifnot( identical( readlines(x), readLines(x) ) )
}
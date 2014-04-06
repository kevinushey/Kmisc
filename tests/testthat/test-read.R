context("read")
library(Rcpp)

dat <- as.data.frame( replicate(10, rnorm(1E4), simplify=FALSE) )
tempfile <- tempfile()
write.table(dat, file=tempfile)

tmp1 <- readLines(tempfile)
tmp2 <- readlines(tempfile)
expect_identical(tmp1, tmp2)

tmp3 <- read(tempfile)

if (!is.null(Sys.info()) && Sys.info()[["sysname"]] == "Windows") {
  nl <- "\r\n"
} else {
  nl <- "\n"
}

expect_identical(tmp2, unlist(strsplit(tmp3, nl, fixed=TRUE)))

# cppFunction(verbose=TRUE, includes="
# #include <Rcpp.h>
# #include <fstream>
# ", code='
# CharacterVector read_lines(std::string path) {
#   using namespace std;
#   ifstream t(path.c_str());
#   string tmp;
#   vector<string> out;
#   while (getline(t, tmp)) {
#     out.push_back(tmp);
#   }
#   return wrap(out);
# }
# ')
#
# rbenchmark::benchmark( replications=5,
#   readlines(tempfile),
#   readLines(tempfile),
#   read_lines(tempfile)
# )
#
# unlink(tempfile)

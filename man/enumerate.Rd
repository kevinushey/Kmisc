\name{enumerate}
\alias{enumerate}
\title{Enumerate over a Vector}
\usage{
enumerate(X, FUN, ...)
}
\arguments{
  \item{X}{A vector.}

  \item{FUN}{A function, taking two arguments.}
}
\description{
This function extends `lapply` to operate on functions
taking two arguments -- the first refers to each element
within a vector, while the second refers to the current
index.
}
\details{
Dot arguments are currently unimplemented.
}
\examples{
data <- setNames( list(1, 2, 3), c('a', 'b', 'c') )
enumerate(data, function(x, i) {
  cat("List element '", i, "' has value '", x, "' and name '",
    names(data)[i], "'.\\n", sep="")
})
}


\name{stack_list}
\alias{stack_list}
\title{Stack a List of DataFrame-like Objects}
\usage{
  stack_list(list, which = TRUE)
}
\arguments{
  \item{list}{a list of data frames.}

  \item{which}{boolean. add a column \code{which} built
  from row names?}
}
\description{
  Function for stacking a list, where each component of the
  list is a \code{data.frame} or a \code{matrix} containing
  potentially differing number of rows, but the same
  columns. The main 'extra' is handling of row names, which
  are passed on into the stacked data frame. These are
  passed into a column called \code{which} to protect from
  problems with non-unique row names, and also to avoid
  appending numbers onto these row names as well.
}
\examples{
x <- data.frame( x=c(1, 2, 3) )
rownames(x) <- c("apple", "banana", "cherry")
y <- data.frame( x=c('a', 'b', 'c') )
rownames(y) <- c("date", "eggplant", "fig")
stack_list( list(x, y) )
}


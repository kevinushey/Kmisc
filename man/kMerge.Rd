\name{kMerge}
\alias{kMerge}
\title{Merge (Left Join) with Order Retainment}
\usage{
  kMerge(x, y, by = intersect(names(x), names(y)),
    by.x = by, by.y = by, ...)
}
\arguments{
  \item{x}{the \code{data.frame} you wish to merge \code{y}
  into.}

  \item{y}{the \code{data.frame} to be merged.}

  \item{by}{specifications of the columns used for merging.
  See 'Details' of \code{\link{merge}}.}

  \item{by.x}{specifications of the columns used for
  merging. See 'Details' of \code{\link{merge}}.}

  \item{by.y}{specifications of the columns used for
  merging. See 'Details' of \code{\link{merge}}.}

  \item{...}{optional arguments passed to \code{merge}.}
}
\value{
  \code{data.frame}
}
\description{
  \code{merge} will mangle the order of the data frames it
  is merging. This is a simple modification to ensure that
  the order in data frame \code{x} is preserved when doing
  a 'left join'; ie, \code{merge( x, y, all.x=TRUE, ... )}.
  That is, if we want to merge a data frame \code{x} with
  another data frame \code{y}, we can merge in the parts of
  \code{y} whose index matches with that of \code{x}, while
  preserving the ordering of \code{x}.
}
\examples{
x <- data.frame( id=5:1, nums=rnorm(5) )
y <- data.frame( id=1:3, labels=c(1, 2, 2) )
merge(x, y, all.x=TRUE) ## re-ordered the data.frame
merge(x, y, all.x=TRUE, sort=FALSE) ## nope - NAs cause problems
kMerge(x, y, by="id") ## preserves ordering of x, even with NAs

## an id entry appears more than once in y
y <- data.frame( id=c(1, 1, 2), labels=c(1, 2, 3) )
kMerge(x, y, by="id")
}
\seealso{
  \code{\link{merge}}
}


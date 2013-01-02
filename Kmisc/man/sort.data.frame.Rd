\name{sort.data.frame}
\alias{sort.data.frame}
\title{Sort a Data Frame}
\usage{
  sort.data.frame(..., na.last = TRUE, decreasing = FALSE)
}
\arguments{
  \item{...}{call supplied to \code{order}}

  \item{na.list}{see \code{\link{order}}}

  \item{decreasing}{see \code{\link{order}}}
}
\description{
  This function sorts a data frame. The syntax is intended
  to mirror \code{order}, such that \code{sort(df, <call>)}
  acts the same as \code{df[ order(df, <call>), ]}.
}
\note{
  Inspired by \code{arrange} in the \code{plyr} package.
}


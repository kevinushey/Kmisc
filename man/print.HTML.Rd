\name{print.HTML}
\alias{print.HTML}
\title{Print HTML Objects}
\usage{
  \method{print}{HTML} (...)
}
\arguments{
  \item{...}{a set of HTML objects (strings).}
}
\description{
  By default, we \code{cat} out HTML objects as we
  typically intend to embed them in \R Markdown documents.
  This is mainly used for printing of items in the
  environment \code{html}.
}
\examples{
Kmisc:::.html$br()
}
\seealso{
  \code{\link{html}}
}


\name{htmlTable}
\alias{htmlTable}
\title{Generate an HTML Table}
\usage{
htmlTable(x, class = "table table-condensed table-hover", id = NULL,
  style = NULL, attr = NULL, output = TRUE, ...)
}
\arguments{
  \item{x}{A \code{data.frame} or \code{matrix}.}

  \item{class}{The CSS class to give the table. By default,
  we use Twitter bootstrap styling -- for this to take
  effect, your document must include bootstrap CSS.}

  \item{id}{The CSS id to give the table.}

  \item{style}{Custom styling to apply to the table.}

  \item{attr}{Other attributes we wish to apply to the
  table.}

  \item{output}{Whether we should write the output to the
  console. We hijack the \code{kable} argument.}

  \item{...}{Optional arguments passed to
  \code{\link{kable}}.}
}
\description{
  This function is used to generate an HTML table; it wraps
  to \code{knitr::kable} but gives some 'extras'; in
  particular, it allows us to set the class, id, and other
  HTML attributes.
}
\examples{
df <- data.frame(`P Values`=runif(1000), Group=1:1000)
htmlTable( head(df[ order(df$P, decreasing=FALSE), ]) )
## wow! look at all that significance! ...
}


\name{makeHTMLTable}
\alias{makeHTMLTable}
\title{Make HTML Table from R 'table-like' Object}
\usage{
  makeHTMLTable(x, attr = NULL, row.spans = 0,
    col.spans = 0, use.row.names = FALSE,
    use.col.names = FALSE, clean = FALSE,
    replace.periods = TRUE)
}
\arguments{
  \item{x}{the \code{data.frame} / \code{matrix} you want
  to convert to an HTML table}

  \item{attr}{attributes to be passed to the \code{<table>}
  tag, as raw HTML.}

  \item{row.spans}{a matrix specifying desired row.spans,
  for largers cells.}

  \item{col.spans}{a matrix specifying desired column
  spans, for larger cells}

  \item{use.row.names}{if you submit an object with row
  names, use those names in construction of the table}

  \item{use.col.names}{if you submit an object with column
  names, use those names in construction of the table}

  \item{clean}{boolean. if \code{TRUE}, rounds all numeric
  values to 2 decimal places for better printing}

  \item{replace.periods}{replace periods with spaces?}
}
\description{
  Function for making HTML tables from an R 'table-like'
  object; ie, a \code{data.frame} or a \code{matrix}. It
  simply parses the item as an HTML table.
}
\details{
  The row.spans and col.spans argument can be specified as
  a matrix to set the row or column span of a certain cell
  to be >1, if desired. See \code{\link{pxt}} for an
  example implementation. It will also handle 'boxes', eg
  cells with both rowspan and colspan > 1.

  Note that the default behavior is to \code{'clean'} your
  data; this rounds numeric output to two decimal places so
  it prints more nicely.
}
\examples{
dat <- data.frame( apple=c(1.2150125, 2, 3), banana=c("a", "b", "c") )
makeHTMLTable( dat )
}


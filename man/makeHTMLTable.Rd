\name{makeHTMLTable}
\alias{makeHTMLTable}
\title{Make HTML Table from R 'table-like' Object}
\usage{
makeHTMLTable(x, attr, row.spans = 0, col.spans = 0,
  use.row.names = FALSE, use.col.names = FALSE, clean = TRUE,
  replace.periods = TRUE)
}
\arguments{
  \item{x}{the \code{data.frame} / \code{matrix} you want
  to convert to an HTML table.}

  \item{attr}{attributes to be passed to the \code{<table>}
  tag, as raw HTML.}

  \item{row.spans}{a matrix specifying desired row.spans,
  for largers cells.}

  \item{col.spans}{a matrix specifying desired column
  spans, for larger cells.}

  \item{use.row.names}{if you submit an object with row
  names, use those names in construction of the table.}

  \item{use.col.names}{if you submit an object with column
  names, use those names in construction of the table.}

  \item{clean}{boolean. if \code{TRUE}, we print all
  numeric values with 4 digits. Alternatively, we can pass
  a format specifier as used by \code{\link{sprintf}}.}

  \item{replace.periods}{replace periods with spaces?}
}
\description{
  Function for making HTML tables from an \R 'table-like'
  object; ie, a \code{data.frame} or a \code{matrix}. It
  simply parses the item as an HTML table.
}
\details{
  The row.spans and col.spans argument can be specified as
  a matrix to set the row or column span of a certain cell
  to be >1, if desired. See \code{\link{pxt}} for an
  example implementation. It will also handle 'boxes', e.g.
  cells with both rowspan and colspan > 1.

  Note that the default behavior is to \code{'clean'}
  numeric input; this prints numeric values with a maximum
  of four digits; ie, through the \code{"\%.4G"} format
  specifier. Alternatively, you can use a format specifier
  (as used in \code{sprintf}) to ensure numbers are
  formatted and displayed as desired.
}
\examples{
dat <- data.frame( apple=c(1.2150125, 2, 3), banana=c("a", "b", "c") )
makeHTMLTable( dat )
}


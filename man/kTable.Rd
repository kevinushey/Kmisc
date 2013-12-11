\name{kTable}
\alias{kTable}
\title{Make Nicely Formatted Tables}
\usage{
kTable(x, y = NULL, deparse.level = 2, top.left.cell = "",
  col.names = NULL, row.names = NULL, left.label = NULL,
  top.label = NULL, google = FALSE)
}
\arguments{
  \item{x}{the \code{x} variable to build a table on.}

  \item{y}{optional: the \code{y} variable to build a table
  on. Used for 2x2 contingency tables.}

  \item{deparse.level}{passed to \code{table};
  \code{deparse.level=2} allows us to pass through variable
  names.}

  \item{top.left.cell}{the string to set in the top left
  cell of the table.}

  \item{col.names}{a vector of column names to use on the
  outputted table; typically this is parsed from the
  variables passed through.}

  \item{row.names}{a vector of row names to use on the
  outputted table; typically this is parsed from the
  variables passed through.}

  \item{left.label}{the label to use for the rows;
  typically parsed from \code{x}. Only used for 2D tables
  (ie, when \code{y} is not null).}

  \item{top.label}{the label to use for the columns;
  typically parsed from \code{y}. Only used for 2D tables
  (ie, when \code{y} is not null).}

  \item{google}{used if you plan on passing the table to
  \code{gvistable} from the \code{googleVis} package.}
}
\description{
Function for creating nice 1D and 2D tables. Tables are
generated and formatted with both counts and percentages.
Primarily intended to be used with R Markdown documents,
calling some of the table printing functions. The function
returns a \code{data.frame} in a format that can be used
with utility HTML generation functions.
}
\examples{
x <- rbinom(100, size=2, p=0.1)
y <- rbinom(100, size=2, p=0.1)

## try these in an R markdown document for best results
kTable(x)
my_table <- kTable(x, y, top.left.cell="foo", left.label="bar", top.label="baz")
pxt( my_table )
}


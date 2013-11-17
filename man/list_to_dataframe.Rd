\name{list_to_dataframe}
\alias{list_to_dataframe}
\alias{list_to_df}
\title{Convert list to data.frame}
\usage{
list_to_dataframe(list, inplace = FALSE)

list_to_df(list, inplace = FALSE)
}
\arguments{
  \item{list}{A list.}

  \item{inplace}{Boolean. If \code{TRUE}, we convert the
  list in place, so that the \code{list} itself is
  transformed into a \code{data.frame}, sans copying.}
}
\description{
  This function converts a list to a data frame, assuming
  that each element of the list is of equal length.
}


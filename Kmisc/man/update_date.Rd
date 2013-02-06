\name{update_date}
\alias{update_date}
\title{Update Date in DESCRIPTION File}
\usage{
  update_date(file = "DESCRIPTION")
}
\arguments{
  \item{file}{The path to the \code{DESCRIPTION} file.}
}
\description{
  This function for package authors updates the time in the
  DESCRIPTION file to the current date, as discovered
  through \code{Sys.Date}().
}


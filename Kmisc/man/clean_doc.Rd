\name{clean_doc}
\alias{clean_doc}
\title{Clean Documentation in Current Package}
\usage{
  clean_doc(dir = getwd())
}
\arguments{
  \item{dir}{the project directory.}
}
\description{
  This function removes all the .Rd documentation files
  present in \code{<dir>/man}. This function is handy if
  you've 'polluted' your manual directory in prototyping
  different functions - assuming that you're documenting
  your code with eg. \code{roxygen}.
}


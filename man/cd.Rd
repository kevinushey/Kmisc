\name{cd}
\alias{cd}
\title{Set Working Directory}
\usage{
cd(...)
}
\arguments{
  \item{...}{the set of strings to paste together. if no
  arguments are submitted, then we return to the home
  directory.}
}
\description{
A small convenience function that wraps \code{file.path}
into a \code{setwd} call.
}
\examples{
x <- "my_favourite_dir"
#setwd( "C:/", x, "really_awesome_stuff" )
## calls setwd( paste( "C:/", x, "really_awesome_stuff", collapse="" ) )
}


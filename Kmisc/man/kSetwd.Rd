\name{kSetwd}
\alias{kSetwd}
\title{Set Working Directory}
\usage{
  kSetwd(...)
}
\arguments{
  \item{...}{the set of strings to paste together}
}
\description{
  A small convenience function that pastes together all
  arguments supplied, then submits those to \code{setwd}.
  If called with no arguments, it resets the directory to
  the home directory.
}
\examples{
x <- "my_favourite_dir"
#setwd( "C:/", x, "/", "really_awesome_stuff" )
## calls setwd( paste( "C:/", x, "really_awesome_stuff", collapse="" ) )
}


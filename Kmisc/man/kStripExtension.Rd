\name{kStripExtension}
\alias{kStripExtension}
\title{Strip File Extension}
\usage{
  kStripExtension(x, lvl = 1)
}
\arguments{
  \item{x}{the file name, including extension}

  \item{lvl}{the number of \code{'.'} used in defining the
  file extension}
}
\description{
  Strips the extension from a file name. By default, we
  assume the extension is separated from the file name by a
  single period; however, the \code{lvl} argument lets us
  specify how many periods are used in forming the file
  extension.
}
\examples{
x <- "path_to_file.tar.gz"
kStripExtension(x, lvl=2)
}


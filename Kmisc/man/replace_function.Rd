\name{replace_function}
\alias{replace_function}
\title{Replace a Function in a Package / Namespace}
\usage{
  replace_function(old, pkg, FUN, compile = TRUE)
}
\arguments{
  \item{old}{The old function name that you wish to
  replace.}

  \item{pkg}{The name of the package / namespace in which
  the function you plan to replace, resides.}

  \item{FUN}{The function definition you plan on using to
  replace \code{old} with.}

  \item{compile}{Boolean, if \code{TRUE} we compile the
  function \code{FUN}.}
}
\description{
  This function can be used to replace the function
  definition in a locked package or namespace with your
  own.
}


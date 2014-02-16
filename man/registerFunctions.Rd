\name{registerFunctions}
\alias{registerFunctions}
\title{Automatically Register C/C++ Functions in a Package}
\usage{
registerFunctions(prefix = "C_")
}
\arguments{
  \item{prefix}{A prefix to append to the exported name, so
  that a function called \code{myfun} is registered as
  \code{<prefix>myfun}.}
}
\description{
This function can be used to automatically register the
native routines in a package. It searches all of the
\code{.c} and \code{.cpp} files in \code{src}, excluding
the file \code{<pkgname>_init.c}, finds functions annotated
with \code{// [[register]]}, and extracts the required
information needed to register routines in the package. The
necessary routines are written to a file called
\code{src/<pkgname>_init.c}.
}
\details{
This function should be called from the base directory of
an \R package you are developing.

Currently, the assumption is that all functions in a
package use the \code{.Call} interface; i.e., there are no
functions using the \code{.C}, \code{.Fortran}, or
\code{.External} interfaces -- this may be added in a
future version.

After calling this function, ensure that you have
\code{useDynLib(<pkg>, .registration=TRUE)} in your
\code{NAMESPACE}. If you use \code{roxygen} to document
your package, you can use

\describe{ \item{ }{\code{##' @useDynLib <pkg>,
.registration=TRUE}} }

somewhere in your \code{roxygen} documentation to achieve
the same effect.
}


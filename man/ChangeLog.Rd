\name{ChangeLog}
\alias{ChangeLog}
\title{Update a Package ChangeLog}
\usage{
ChangeLog(msg = "")
}
\arguments{
  \item{msg}{A message to use for the first file.}
}
\description{
This function compares the time that files were last
modified with the time that the current \R session was
started (well, the time that \code{Kmisc} was loaded),
generates stubs for them in a file called \code{ChangeLog},
and then opens that file in an editor.
}


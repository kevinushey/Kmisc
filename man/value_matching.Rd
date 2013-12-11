\name{value_matching}
\alias{\%kin\%}
\alias{\%knin\%}
\alias{\%nin\%}
\alias{value_matching}
\title{Value Matching}
\usage{
x \%nin\% y

x \%kin\% y

x \%knin\% y
}
\arguments{
  \item{x}{Vector or \code{NULL}: the values to be
  matched.}

  \item{y}{Vector or \code{NULL}: the values to be matched
  against.}
}
\description{
These are a couple of mostly self-explanatory wrappers
around \code{\%in\%}.
}
\details{
\code{\%nin\%} returns a logical vector indicating if there
is no match for its left operand. It is the inverse of
\code{x \%in\% y}.

\code{\%kin\%} returns the actual values of \code{x} for
which \code{x \%in\% y}.

\code{\%knin\%} returns the actual values of \code{x} for
which \code{x \%nin\% y}.
}


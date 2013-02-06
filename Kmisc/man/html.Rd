\name{html}
\alias{html}
\title{Print HTML Elements}
\usage{
  html(..., file = "")
}
\arguments{
  \item{...}{A set of HTML tag functions. See examples for
  details.}

  \item{file}{Location to output the generated HTML.}
}
\description{
  Use this function to output HTML code for use in R
  Markdown documents or otherwise.
}
\examples{
html(
  h1("Welcome!"),
  div(class="header", table( tr( td("nested elements are ok") ) ) ),
  footer(class="foot", "HTML5 footer")
  )
}
\seealso{
  \code{\link{makeHTMLTag}}, for making your own tags.
}


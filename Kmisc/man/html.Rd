\docType{data}
\name{html}
\alias{html}
\title{HTML Tags}
\format{An environment of functions, with names corresponding to their tag.}
\description{
  This is an environment containing R functions useful in
  generating HTML tags. See examples for usage.
}
\examples{
## The first argument is interpreted as the 'data' of an HTML element, while
## all further arguments are interpreted as named attributes to pass to
## the HTML tag you're using
data(html)
myName <- "Kevin"
with( html, strong( paste0("Hi, ", myName, "!" ) ) )

with( html, table( class="my-favorite-table",
  tr( td(myName, class="blue") ) )
  )

## See all of the HTML tags currently available
print( objects(html) )
}
\keyword{datasets}


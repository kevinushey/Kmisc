\docType{data}
\name{.html}
\alias{.html}
\title{HTML Tags}
\format{An environment of functions, with names corresponding to their tag.}
\description{
  This is an environment containing R functions useful in
  generating HTML tags. See examples for usage.
}
\examples{
## All un-named arguments are interpreted as 'data' to paste (in order),
## while all named arguments are interpreted as HTML attributes to give to
## the tag.
myName <- "Kevin"
html( strong( style="color: red;", "Hi, ", myName, "!" ) )

html( table( id="my-favorite-table",
  tr( td(myName, class="blue") ) )
  )

## See all of the HTML tags currently available
print( objects(.html) )

## make your own!
article <- makeHTMLTag("article")
article( id="main", "hello!" )
}
\keyword{datasets}


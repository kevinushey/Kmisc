\name{makeHTMLTag}
\alias{makeHTMLTag}
\title{Make HTML Elements}
\usage{
  makeHTMLTag(tag, ...)
}
\arguments{
  \item{tag}{the HTML tag to use}

  \item{...}{a collection of named and unnamed arguments;
  named arguments are parsed as attributes of the tag,
  unnamed arguments are pasted together into the inner data
  of the tag.}
}
\description{
  Creates a function that returns a function that can be
  used to generate HTML elements. See examples for usage.
}
\details{
  This function returns a function that can be called as an
  HTML tag generating function. For example, by calling
  \code{p <- makeHTMLTag("p")}, we can generate a function
  that interprets all named arguments as attributes, and
  all unnamed arguments as 'data', which is generated for a
  \code{p} HTML tag.
}
\examples{
div <- makeHTMLTag("div")
my_class = "orange"
x <- "some text"
div( class=my_class, id="hello", "This is ", x )
}
\seealso{
  \code{\link{html}}
}


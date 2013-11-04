##' Print HTML Elements
##' 
##' Use this function to output HTML code for use in R Markdown documents
##' or otherwise.
##' 
##' @param ... A set of HTML tag functions. See examples for details.
##' @param file Location to output the generated HTML.
##' @param .sub A named list of substitutions to perform; we substitute each
##'   symbol from the names of \code{.sub} with its corresponding value.
##' @export
##' @seealso
##' \code{\link{makeHTMLTag}}, for making your own tags.
##' @examples
##' html(
##'   h1("Welcome!"),
##'   div(class="header", table( tr( td("nested elements are ok") ) ) ),
##'   footer(class="foot", "HTML5 footer")
##' )
html <- function(..., file="", .sub=NULL) {
  dotArgs <- match.call(expand.dots=FALSE)$`...`
  
  if (!is.null(.sub)) {
    if (!is.list(.sub)) stop(".sub must be a list")
    if (is.null(names(.sub))) stop("the names of .sub cannot be NULL")
    dotArgs <- lapply(dotArgs, function(x) {
      do.call(substitute, list(x, .sub))
    })
  }
  
  for (item in dotArgs) {
    item <- .eval_symbols(item)
    print( eval(item, envir=.html), file=file )
    cat( "\n", file=file )
  }
}

.eval_symbols <- function(lang, envir=parent.frame()) {
  if (is.call(lang) && length(lang) > 1 && as.character(lang[[1]]) %nin% c("[", "[[", "$")) {
    for (i in 2:length(lang)) {
      if (is.call(lang[[i]])) {
        lang[[i]] <- .eval_symbols( lang[[i]], envir=envir )
      } else {
        lang[[i]] <- eval( lang[[i]], envir=envir )
      }
    }
  } else if (is.symbol(lang)) {
    lang <- eval(lang, envir=envir)
  }
  return(lang)
}

##' Print kHTML Objects
##' 
##' By default, we \code{cat} out kHTML objects as we typically
##' intend to embed them in \R Markdown documents. This is mainly used for
##' printing of items in the environment \code{html}.
##' @param ... a set of kHTML objects (strings).
##' @method print kHTML
##' @S3method print kHTML
##' @seealso \code{\link{html}}
##' @examples
##' Kmisc:::.html$br()
print.kHTML <- function(...) {
  cat(..., "\n")
}

##' Make HTML Elements
##' 
##' Creates a function that returns a function that can be used to generate
##' HTML elements. See examples for usage.
##' 
##' This function returns a function that can be called as an HTML tag
##' generating function. For example, by calling
##' \code{p <- makeHTMLTag("p")}, we can generate a function that interprets
##' all named arguments as attributes, and all unnamed arguments as
##' 'data', which is generated for a \code{p} HTML tag.
##' 
##' @export
##' @param tag the HTML tag to use.
##' @param ... a collection of named and unnamed arguments;
##'  named arguments are parsed as attributes of the tag,
##'  unnamed arguments are pasted together into the inner data of the tag.
##' @seealso \code{\link{html}}
##' @examples
##' div <- makeHTMLTag("div")
##' my_class = "orange"
##' x <- "some text"
##' div( class=my_class, id="hello", "This is ", x )
makeHTMLTag <- function(tag, ...) {
  
  ## force tag into a closure
  force(tag)
  
  return( function(...) {
    
    ## get dot args
    dotArgs <- list(...)
    
    ## process attributes from named arguments
    preparsedArgs <- as.list( match.call(expand.dots=FALSE)$`...` )
    namedArgs <- dotArgs[ names( preparsedArgs ) != "" ]
    
    if( length( namedArgs ) > 0 ) {
      attrs <- paste( sep="", " ", paste( names(namedArgs), 
        paste( sep="",  "'",
          unlist( namedArgs ),
          "'" ),
        sep = "=", collapse = " " ) )
    } else {
      attrs <- NULL
    }
    
    ## process data from unnamed arguments
    if( is.null( names( preparsedArgs ) ) ) {
      unnamedArgs <- dotArgs
    } else {
      unnamedArgs <- dotArgs[ names(preparsedArgs) == "" ]
    }
    
    data <- paste( unnamedArgs, sep="", collapse="" )
    
    if( length( unnamedArgs ) == 0 ) {
      out <- paste( "<", tag, attrs, " />", sep="", collapse="" )
    } else {
      out <- paste( "<", tag, attrs, ">\n", data, "\n</", tag, ">",
        sep="", collapse="")
    }
    
    class(out) <- "kHTML"
    return(out)
    
  } )
  
}

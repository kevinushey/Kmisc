##' @importFrom markdown markdownToHTML
Kmisc.markdownToHTML <- function (file, output, text, options = getOption("markdown.HTML.options"), 
  extensions = getOption("markdown.extensions"), title = "", 
  stylesheet = getOption("markdown.HTML.stylesheet"), header = getOption("markdown.HTML.header"), 
  template = getOption("markdown.HTML.template"), fragment.only = FALSE) 
{
  
  html <- markdownToHTML(file=file, output=NULL, text=text, options=options,
    extensions=extensions, title=title, stylesheet=stylesheet, header=header,
    template=template, fragment.only=fragment.only)
  
  if (missing(output)) {
    output <- paste0( gsub("\\.[Mm][Dd]$", "", file), ".html" )
  }
  
  .addLibrary <- function(text, file, html) {
    return( sub(text, read(file), html, fixed=TRUE) )
  }
  
  html <- .addLibrary("#!bootstrap_css#",
    system.file("resources/bootstrap/css/bootstrap.css", package="Kmisc"),
    html
  )
  
  html <- .addLibrary("#!Kmisc_css#",
    system.file("resources/css/Kmisc.css", package="Kmisc"),
    html
  )
  
  html <- .addLibrary("#!tocify_css#",
    system.file("resources/css/jquery.tocify.css", package="Kmisc"),
    html
  )
  
  html <- .addLibrary("#!jquery#",
    system.file("resources/js/jquery-2.0.3.min.js", package="Kmisc"),
    html
  )
  
  html <- .addLibrary("#!jqueryui#",
    system.file("resources/js/jquery-ui-1.10.3.custom.min.js", package="Kmisc"),
    html
  )
  
  html <- .addLibrary("#!bootstrap_js#",
    system.file("resources/bootstrap/js/bootstrap.min.js", package="Kmisc"),
    html
  )
  
  html <- .addLibrary("#!tocify_js#",
    system.file("resources/js/jquery.tocify.min.js", package="Kmisc"),
    html
  )
  
  html <- .addLibrary("#!fancyboxify#",
    system.file("resources/js/fancyboxify.js", package="Kmisc"),
    html
  )
  
  html <- .addLibrary("#!highlight_js#",
    system.file("resources/highlight/highlight.pack.js", package="Kmisc"),
    html
  )
  
  html <- .addLibrary("#!highlight_css#",
    system.file("resources/highlight/styles/tomorrow-night-bright.css", package="Kmisc"),
    html
  )
  
  ret <- html
  
  if (is.character(output)) {
    writeLines(ret, output)
    ret <- NULL
  }
  
  invisible(ret)
  
}

##' Knit an Rmd File to HTML with Kmisc Styling
##' 
##' This function 'knits' an R Markdown document with \code{knitr}, and injects
##' HTML, CSS and JavaScript from CSS for nice, interactive HTML reports.
##' 
##' @param input An \code{.Rmd} file.
##' @param ... Optional arguments passed to \code{knit}.
##' @importFrom knitr knit
##' @export
Kmisc.knit2html <- function(input, ...) {
  output <- knit(input, envir=.GlobalEnv, ...)
  Kmisc.markdownToHTML(output)
}

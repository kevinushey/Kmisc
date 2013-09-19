##' Generate an HTML Table
##' 
##' This function is used to generate an HTML table; it wraps to
##' \code{knitr::kable} but gives some 'extras'; in particular, it allows
##' us to set the class, id, and other HTML attributes.
##' 
##' @importFrom knitr kable
##' @param x A \code{data.frame} or \code{matrix}.
##' @param class The CSS class to give the table. By default, we use Twitter
##'   bootstrap styling -- for this to take effect, your document must include
##'   bootstrap CSS.
##' @param id The CSS id to give the table.
##' @param style Custom styling to apply to the table.
##' @param attr Other attributes we wish to apply to the table.
##' @param output Whether we should write the output to the console. We hijack
##'   the \code{kable} argument.
##' @param ... Optional arguments passed to \code{\link{kable}}.
##' @export
##' @examples
##' df <- data.frame(`P Values`=runif(1000), Group=1:1000)
##' htmlTable( head(df[ order(df$P, decreasing=FALSE), ]) )
##' ## wow! look at all that significance! ...
htmlTable <- function(x, 
  class="table table-condensed table-hover", 
  id=NULL, 
  style=NULL, 
  attr=NULL, 
  output=TRUE,
  ...) {
  tbl <- kable(x, format='html', output=FALSE, ...)
  tbl_tag <- paste0("<table ",
    if (!is.null(class)) paste0("class='", class, "' "),
    if (!is.null(id)) paste0("id='", id, "' "),
    if (!is.null(style)) paste0("style='", style, "' "),
    if (!is.null(attr)) attr,
    ">"
  )
  if (tbl_tag == "<table >") tbl_tag <- "<table>" ## pedantic
  tbl <- sub("<table>", tbl_tag, tbl, fixed=TRUE)
  if (output) {
    cat(tbl)
  }
  invisible(tbl)
}

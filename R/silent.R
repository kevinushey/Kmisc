##' Hide All Potential Output from an Expression
##' 
##' This function combines hides any output a function might try to write
##' to \code{stdout}, \code{stderr}, to the \R console as a message, warning,
##' and so on.
##' 
##' @param expr An \R expression.
##' @export
silent <- function(expr, envir=parent.frame(1), enclos=baseenv()) {
  call <- match.call()
  suppressMessages( suppressWarnings(
    capture.output(output <- eval(call$expr, envir=envir, enclos=enclos))
  ) )
  return(output)
}

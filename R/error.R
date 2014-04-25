##' Stop / Warning / Message Wrappers
##' 
##' These are simple wrappers to functions \code{stop}, \code{warning}, and
##' \code{message}.
##' 
##' @param expr An expression to be evaluated and checked for 'truthiness'.
##' @param fmt A character vector of format strings. Passed to \code{\link{gettextf}}.
##' @param ... Optional arguments passed to \code{\link{gettextf}}.
##' @name error-wrappers
NULL

##' @rdname error-wrappers
##' @export
stopf <- function(fmt, ...) {
  stop(gettextf(fmt, ...), call.=FALSE) 
}

##' @rdname error-wrappers
##' @export
warnf <- function(fmt, ...) {
  warning(gettextf(fmt, ...))
}

##' @rdname error-wrappers
##' @export
messagef <- function(fmt, ...) {
  message(gettextf(fmt, ...))
}

##' @rdname error-wrappers
##' @export
stop_if <- function(expr, fmt, ...) {
  call <- match.call()
  if (eval(expr)) {
    stopf(gettextf(fmt, ...))
  }
}

##' @rdname error-wrappers
##' @export
warn_if <- function(expr, fmt, ...) {
  call <- match.call()
  if (eval(expr)) {
    warnf(gettextf(fmt, ...))
  }
}

##' @rdname error-wrappers
##' @export
message_if <- function(expr, fmt, ...) {
  call <- match.call()
  if (eval(expr)) {
    message(gettextf(fmt, ...))
  }
}

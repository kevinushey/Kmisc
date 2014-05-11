##' Python-style Dictionaries in R
##'
##' Dictionaries, i.e., hashed key-value pairs, are implemented in \R using
##' environments as a backend.
##'
##' Dictionaries are just hashed \R environments with \code{emptyenv()} as a
##' parent.
##'
##' @param ... Named arguments used in constructing the dictionary.
##' @param `_size` The number of 'buckets' used. This is the maximum of
##'   \code{29L} and the number of named arguments passed to \code{...}.
##' @export
##' @examples
##' ## Named lookup can be much faster in a dictionary
##' x <- as.list(1:1E5)
##' names(x) <- paste0("Element_", 1:1E5)
##' dict <- as.dictionary(x)
##' if (require(microbenchmark)) {
##'   microbenchmark(
##'     x[["Element_1"]],
##'     dict[["Element_1"]],
##'     x[["Element_100000"]],
##'     dict[["Element_100000"]]
##'   )
##' }
dict <- function(..., `_size` = 29L) {
  dots <- list(...)
  dict <- new.env(parent = emptyenv(), size = max(`_size`, length(dots)))
  names <- names(dots)
  for (i in seq_along(names)) {
    dict[[ names[i] ]] <- dots[[i]]
  }
  class(dict) <- "dictionary"
  dict
}

##' @export
print.dictionary <- function(x, ...) {
  cat("Dictionary of ", length(x), " key-value pairs\n", sep = "")
}

##' Coerce an Object to a Dictionary
##' @export
as.dictionary <- function(x, ...) {
  UseMethod("as.dictionary")
}

##' @export
str.dictionary <- function(object, ...) {
  print(object, ...)
  keys <- keys(object)
  invisible(enumerate(keys, function(x, i) {
    cat(" $ ", x, ":", sep="")
    str(get(keys[[i]], envir = object))
  }))
}

##' @export
as.dictionary.list <- function(x, ...) {
  env <- list2env(x, parent = emptyenv(), hash = TRUE, size = max(29L, length(x)))
  class(env) <- "dictionary"
  env
}

##' @export
keys <- function(x) {
  UseMethod("keys")
}

##' @export
keys.dictionary <- function(x) {
  objects(x)
}

##' @export
values <- function(x) {
  UseMethod("values")
}

##' @export
values.dictionary <- function(x) {
  lapply(keys(x), function(elem) get(elem, envir = x))
}

##' @export
`[.dictionary` <- function(x, i, j, ..., drop = FALSE) {
  lapply(i, function(key) x[[key]])
}

##' @export
`[<-.dictionary` <- function(x, i, j, value) {
  invisible(lapply(i, function(key) {
    x[[key]] <- value
  }))
  x
}

##' Python-style Dictionaries in R
##'
##' Dictionaries, i.e., hashed key-value pairs, are implemented in \R using
##' environments as a backend.
##'
##' Dictionaries are just hashed \R environments with \code{emptyenv()} as a
##' parent.
##'
##' @section Warning:
##'
##' Dictionaries have \bold{reference semantics}, so modifying a dictionary
##' within a function will modify the dictionary passed in, not a copy! Use the
##' \code{copy} function to duplicate a \code{dict}.
##'
##'
##' @param ... Named arguments used in constructing the dictionary.
##' @param `_size` The number of 'buckets' used. This is the maximum of
##'   \code{29L} and the number of named arguments passed to \code{...}.
##' @export
##' @examples
##' ## Reference semantics -- be careful!
##' x <- dict()
##' y <- x
##' x[["a"]] <- 100
##' print(y[["a"]])
##'
##' ## Use copy to be explicit
##' y <- copy(x)
##' x[["b"]] <- 200
##' try(y[["b"]], silent = TRUE)
##'
##' ## Named lookup can be much faster in a dictionary
##' x <- as.list(1:1E5)
##' names(x) <- paste0("Element_", 1:1E5)
##' dict <- as.dictionary(x)
##' if (require(microbenchmark)) {
##'   microbenchmark(
##'     x[["Element_1"]],
##'     dict[["Element_1"]],
##'     x[["Element_1000"]],
##'     dict[["Element_1000"]],
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
  mget(i, envir = x, inherits = FALSE)
}

##' @export
`[[.dictionary` <- function(x, i, j, ..., drop = FALSE) {
  get(i, envir = x, inherits = FALSE)
}

##' @export
`[<-.dictionary` <- function(x, i, j, value) {
  invisible(lapply(i, function(key) {
    x[[key]] <- value
  }))
  x
}

##' @export
copy <- function(x) UseMethod("copy")

##' @export
copy.dictionary <- function(x) {
  output <- list2env(
    x = as.list.environment(x, all.names = TRUE),
    parent = emptyenv(),
    hash = TRUE
  )
  class(output) <- "dictionary"
  output

}

#' Loading Packages
#' 
#' These functions exist to dissuade any grumps out there who get mad when
#' you call an \R package an \R library. Now we can load a package with
#' the function \code{package}. \code{import} is used for the Python users.
#' @export
#' @param package The name of a package, given as a \code{\link{name}} or literal
#' character string, or a character string, depending on whether
#' \code{character.only} is \code{FALSE} (default) or \code{TRUE}.
#' @param help See \code{package}.
#' @param pos The position on the search list at which to attach the loaded
#' package. Can also be the name of a position on the current search list as given
#' by \code{\link{search}()}.
#' @param lib.loc A character vector describing the location of \R library
#' trees to search through, or \code{NULL}. The default value of \code{NULL}
#' corresponds to all libraries currently known to \code{\link{.libPaths}()}.
#' Non-existent libraries trees are silently ignored.
#' @param character.only A logical indicating whether \code{package} or
#' \code{help} can be assumed to be character strings.
#' @param logical.return logical; if \code{TRUE}, successful loading of the
#' package will return \code{TRUE}, and \code{FALSE} otherwise.
#' @param warn.conflicts logical; if \code{TRUE}, warnings are printed about
#' \code{\link{conflicts}} from attaching the new package. A conflict is
#' a function masking a function, or a non-function masking a non-function.
#' @param quietly logical; if \code{TRUE}, no messages confirming package
#' loading is printed, and most often, no errors/warnings are printed if
#' package loading fails.
#' @param keep.source Ignored.
#' @param verbose logical; if \code{TRUE}, additional diagnostics are printed.
#' @seealso \code{\link{library}}
package <- library

#' @rdname package
#' @export
import <- library
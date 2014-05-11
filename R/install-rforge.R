##' Attempts to install a package directly from R-Forge.
##'
##' @param project project name
##' @param ... Other arguments passed on to \code{\link{install.packages}}.
##' @export
##' @examples
##' \dontrun{
##' install_rforge("data.table")
##' }
install_rforge <- function(project, ...) {
  install.packages(project, repos = "http://R-Forge.R-project.org", ...)
}

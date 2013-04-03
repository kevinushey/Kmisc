#' Install R Package from SVN Repository
#' 
#' Use this function to install an R package from an SVN repository. This
#' requires an up-to-date version of \code{svn} on your machine.
#' 
#' @examples
#' ## install an up-to-date version of Rcpp from SVN repository
#' install_svn("svn://svn.r-forge.r-project.org/svnroot/rcpp/pkg/Rcpp", "Rcpp")
install_svn <- function(url, pkg_name) {
  dir <- tempdir()
  owd <- getwd()
  on.exit( setwd(owd) )
  setwd(dir)
  system( paste("svn checkout", url) )
  build_output <- system( paste("R CMD build --no-vignettes --no-manual", pkg_name, intern=TRUE) )
  pkg_src <- build_output[ length(build_output) ]
  pkg_src <- substring( pkg_src, 13, nchar(pkg_src)-1 )
  system( paste("R CMD install --no-multiarch", pkg_src) )
}
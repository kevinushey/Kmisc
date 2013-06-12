#' Install R Package from SVN Repository
#' 
#' Use this function to install an R package from an SVN repository. This
#' requires an up-to-date version of \code{svn} on your machine.
#' 
#' @examples
#' ## install the latest SVN version of Rcpp
#' install_svn("svn://svn.r-forge.r-project.org/svnroot/rcpp/pkg/Rcpp")
install_svn <- function(url, pkg_name, build_opts="", install_opts="") {
  if( missing(pkg_name) ) {
    pkg_name <- gsub(".*/", "", url)
  }
  dir <- tempdir()
  owd <- getwd()
  on.exit( setwd(owd) )
  setwd(dir)
  system( paste("svn checkout", url) )
  system( paste("R CMD build --no-vignettes --no-manual", pkg_name) )
  pkg_file <- grep( paste0("^", pkg_name, "_(.*?)\\.tar\\.gz$"), list.files(), perl=TRUE, value=TRUE )
  stopifnot( length(pkg_file) == 1 )
  system( paste("R CMD install --no-multiarch", pkg_file) )
}
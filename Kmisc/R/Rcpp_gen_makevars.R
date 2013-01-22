#' Reproduce Rcpp Makevars Files
#' 
#' If you're building a package and want a simple set of
#' Makevars files to export, this function will handle it
#' for you.
#' @param src the location to output the Makevars.
Rcpp_gen_makevars <- function(src=file.path( getwd(), "src" )) {
  
  require("Rcpp")
  
  skeleton <- system.file("skeleton", package = "Rcpp")
  Makevars <- file.path(src, "Makevars")
  if (!file.exists(Makevars)) {
    file.copy(file.path(skeleton, "Makevars"), Makevars)
    message(" >> added Makevars file with Rcpp settings")
  }
  Makevars.win <- file.path(src, "Makevars.win")
  if (!file.exists(Makevars.win)) {
    file.copy(file.path(skeleton, "Makevars.win"), Makevars.win)
    message(" >> added Makevars.win file with Rcpp settings")
  }
  
}
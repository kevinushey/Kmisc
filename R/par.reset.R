.par.orig <- par()

##' Restore the 'par' settings
##' 
##' If you have been mucking around with parameter settings, this function
##' will revert to the parameter settings that were available when this
##' package was loaded.
##' @export
par.reset <- function() {
  par(.par.orig)
}

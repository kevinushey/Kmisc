## Overwrite a Symbol in a Package
##
## Replaces a symbol with name \code{x} with a symbol \code{value}, in the
## package \code{package}.
##
## The value of the old symbol is returned.
replace <- function(x, value, package) {
  
  package_ <- asNamespace(package)
  
  old <- tryCatch(
    get(x, envir=package_),
    error=function(e) NULL
  )
  
  pkg_env <- as.environment(paste("package", package, sep=":"))
  environment(value) <- package_
  
  ## Unlock bindings
  tryCatch( unlockBinding(x, pkg_env), error=function(e) {
    stop("Could not unlock binding: no symbol '", x, "' in namespace '", package, "'?")
  })
  tryCatch( unlockBinding(x, package_), error=function(e) {
    stop("Could not unlock binding: no symbol '", x, "' in namespace '", package, "'?")
  })
  
  ## Assign values
  assign(x, value, envir=pkg_env)
  utils::assignInNamespace(x, value, package=package_, envir=pkg_env)
  
  ## Lock bindings
  lockBinding(x, pkg_env)
  lockBinding(x, package_)
  
  ## Return the old function
  return(old)
}

##' Replace a Function in a Package / Namespace
##' 
##' This function can be used to replace the function definition in a locked
##' package or namespace with your own.
##' 
##' @param old The old function name that you wish to replace.
##' @param pkg The name of the package / namespace in which the function
##'   you plan to replace, resides.
##' @param FUN The function definition you plan on using to replace
##'   \code{old} with.
##' @param compile Boolean, if \code{TRUE} we compile the function \code{FUN}.
replace_function <- function(old, pkg, FUN, compile=TRUE) {
  
  if (!is.character(old)) {
    old <- as.character(match.call()$old)
  }
  
  if (!is.character(pkg)) {
    pkg <- as.character(match.call()$pkg)
  }
  
  if (length(old) != 1) {
    stop("'old' must be a character vector of length one")
  }
  
  FUN <- match.fun(FUN)
  
  env <- as.environment( paste0("package:", pkg) )
  ns <- asNamespace(pkg)
  
  environment(FUN) <- ns
  if (compile)
    FUN <- compiler::cmpfun(FUN)
  
  unlockBinding(old, env)
  unlockBinding(old, ns)
  assign(old, FUN, envir=env)
  utils::assignInNamespace(old, FUN, ns=pkg, envir=env)
  lockBinding(old, env)
  lockBinding(old, ns)
  
}

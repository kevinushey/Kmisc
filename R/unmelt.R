##' Unmelt a Melted Data Frame
##' 
##' This function undoes the \code{melt}ing process done by either
##' \code{reshape2::melt} or \code{\link{melt_}}.
##' 
##' @param data A \code{data.frame}.
##' @param variable The index, or name, of the \code{variable} vector; analogous to
##'   the vector produced with name \code{variable.name}.
##' @param value The value of the \code{value} vector; analogous to
##'   the vector produced with name \code{value.name}.
##' @rdname unmelt
##' @export
unmelt_ <- function(data, variable="variable", value="value") {
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  
  if (length(variable) != 1) {
    stop("'variable' must have length one")
  }
  
  if (length(value) != 1 ) {
    stop("'value' must have length 1")
  }
  
  if (is.character(variable)) {
    idm <- match(variable, names(data))
    if (is.na(idm)) {
      stop("No variable with name '", variable, "' in 'data'")
    }
    variable <- idm
  }
  
  if (is.character(value)) {
    valuem <- match(value, names(data))
    if (is.na(value)) {
      stop("No variable with name '", value, "' in 'data'")
    }
    value <- valuem
  }
  
  if (variable < 0 || variable > length(data)) {
    stop("'variable' variable indexes outside of range of 'data'")
  }
  
  if (value < 0 || value > length(data)) {
    stop("'value' variable indexes outside of range of 'data'")
  }
  
  other <- (1:ncol(data))[-c(variable, value)]
  
  return( .Call(Cunmelt,
    data,
    unique(data[[variable]]),
    as.integer(other)-1L,
    as.integer(variable)-1L,
    as.integer(value)-1L
  ) )
  
}

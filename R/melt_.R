##' Make a 'Wide' data set 'Long'
##' 
##' Inspired by \code{reshape2:::melt}, we melt \code{data.frame}s and 
##' \code{matrix}s. This function is built for speed.
##' 
##' If items to be stacked are not of the same internal type, they will be
##' promoted in the order \code{logical} > \code{integer} > \code{numeric} > 
##' \code{character}.
##' 
##' @importFrom data.table alloc.col is.data.table
##' @param data The \code{data.frame} to melt.
##' @param ... Arguments passed to other methods.
##' @examples
##' n <- 20
##' tmp <- data.frame( stringsAsFactors=FALSE,
##'   x=sample(letters, n, TRUE), 
##'   y=sample(LETTERS, n, TRUE),
##'   za=rnorm(n), 
##'   zb=rnorm(n), 
##'   zc=rnorm(n)
##' )
##'   
##' stopifnot( 
##'   identical( 
##'     melt_(tmp, id.vars=c('x', 'y')), 
##'     melt_(tmp, measure.vars=c('za', 'zb', 'zc')) 
##'   ) 
##' )
##' @export
melt_ <- function(data, ...) {
  UseMethod("melt_")
}

##' @rdname melt_
##' @param id.vars Vector of id variables. Can be integer (variable position)
##'  or string (variable name). If blank, we use all variables not in \code{measure.vars}.
##' @param measure.vars Vector of measured variables. Can be integer (variable position)
##'  or string (variable name). If blank, we use all variables not in \code{id.vars}.
##' @param variable.name Name of variable used to store measured variable names.
##' @param value.name Name of variable used to store values.
##' @method melt_ data.frame
##' @S3method melt_ data.frame
melt_.data.frame <- function(data, id.vars, measure.vars, variable.name="variable", ..., value.name="value") {
  
  ## figure out which variables belong to id.vars, measure.vars,
  if( missing(measure.vars) ) {
    if( missing(id.vars) ) {
      ## assume that we are going to melt everything
      id.vars <- integer(0)
    }
    if( is.character(id.vars) ) {
      id.vars <- match( id.vars, names(data) )
    }
    measure.vars <- which( 1:length(data) %nin% id.vars )
  }
  
  if( missing(id.vars) ) {
    if( missing(measure.vars) ) {
      stop("if 'id.vars' is missing, you must supply 'measure.vars'")
    }
    if( is.character(measure.vars) ) {
      measure.vars <- match( measure.vars, names(data) )
    }
    id.vars <- which( 1:length(data) %nin% measure.vars )
  }
  
  if (is.character(id.vars)) {
    id.vars <- match(id.vars, names(data))
  }
  
  if (is.character(measure.vars)) {
    measure.vars <- match(measure.vars, names(data))
  }
  
  if (is.null(id.vars)) {
    id.vars <- integer(0)
  }
  
  if (any_na(id.vars)) {
    stop("Failed to match all of 'id.vars' to variable names in 'data'")
  }

  if (any_na(measure.vars)) {
    stop("Failed to match all of 'measure.vars' to variable names in 'data'")
  }
  
  if (any(id.vars < 1) || any(id.vars > length(data))) {
    stop("one or more of the 'id.vars' indexes beyond column range of data")
  }
  
  if (any(measure.vars < 1) || any(measure.vars > length(data))) {
    stop("one or more of the 'measure.vars' indexes beyond column range of data")
  }
  
  output <- .Call(Cmelt_dataframe,
    data,
    as.integer(id.vars-1L),
    as.integer(measure.vars-1L),
    variable.name,
    value.name
  )
  
  ## preserve data.table-ness
  if (is.data.table(data)) {
    setattr(output, "class", c("data.table", "data.frame"))
    alloc.col(output)
  }
  
  return(output)
  
}

##' @rdname melt_
##' @method melt_ matrix
##' @S3method melt_ matrix
melt_.matrix <- function( data, ... ) {
  return( .Call(Cmelt_matrix, data) )
}

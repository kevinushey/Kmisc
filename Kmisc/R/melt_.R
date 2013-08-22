##' Make a 'Wide' data set 'Long'
##' 
##' Inspired by \code{reshape2:::melt}, we melt \code{data.frame}s and 
##' \code{matrix}s. This function is built for speed.
##' 
##' If items to be stacked are not of the same internal type, they will be
##' promoted in the order \code{logical} > \code{integer} > \code{numeric} > 
##' \code{character}.
##' 
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
melt_.data.frame <- function(data, id.vars, measure.vars, variable.name="name", ..., value.name="value") {
  
  ## figure out which variables belong to id.vars, measure.vars,
  ## if one of them is missing
  
  if( !missing(id.vars) && identical(id.vars, "row.names") ) {
    if( missing(measure.vars) ) {
      measure.vars <- names(data)
    }
  } else {
    
    if( missing(measure.vars) ) {
      if( missing(id.vars) ) {
        stop("one of 'id.vars' and 'measure.vars' must be supplied")
      }
      if( is.character(id.vars) ) {
        measure.vars <- which( names(data) %nin% id.vars )
      } else {
        measure.vars <- which( 1:length(data) %nin% id.vars )
      }
    }
    
    if( missing(id.vars) ) {
      if( missing(measure.vars) ) {
        stop("one of 'id.vars' and 'measure.vars' must be supplied")
      }
      if( is.character(measure.vars) ) {
        id.vars <- which( names(data) %nin% measure.vars )
      } else {
        id.vars <- which( 1:length(data) %nin% measure.vars )
      }
    }
    
  }
  
  ## coerce factors to characters
  if( any( sapply( 1:ncol(data), function(i) {
    is.factor( data[[i]] )
  } ) ) ) {
    warning("factors coerced to characters")
    data <- factor_to_char(data)
  }
  
  ## check and coerce the types of the measure.vars
  types <- sapply( measure.vars, function(x) {
    typeof( data[[x]] )
  } )
  
  if( length( unique( types ) ) > 1 ) {
    if( "character" %in% types ) {
      warning("Coercing types of measure vars to 'character'")
      data[measure.vars] <- lapply( data[measure.vars], as.character )
    } else if( "double" %in% types ) {
      warning("Coercing types of measure vars to 'numeric'")
      data[measure.vars] <- lapply( data[measure.vars], as.numeric )
    } else if( "integer" %in% types ) {
      warning("Coercing types of measure vars to 'integer'")
      data[measure.vars] <- lapply( data[measure.vars], as.integer )
    } else {
      stop("Unhandled type in the measure vars of your data")
    }
  }
  
  ## separate dispatch for row.names
  if( identical(id.vars, "row.names") ) {
    output <- .Call("melt_dataframe",
      data.frame( x=attr(data, "row.names") ),
      data[measure.vars],
      variable.name,
      value.name,
      PACKAGE="Kmisc"
    )
    
    return( output[2:ncol(output)] )
    
  } else {
    return( .Call("melt_dataframe",
      data[id.vars],
      data[measure.vars],
      variable.name,
      value.name,
      PACKAGE="Kmisc"
    ) )
  }
  
}

##' @rdname melt_
##' @method melt_ matrix
##' @S3method melt_ matrix
melt_.matrix <- function( data, ... ) {
  return( .Call("melt_matrix", data, PACKAGE="Kmisc") )
}

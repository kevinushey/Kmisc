#' Melt a Data Frame
#' 
#' Inspired by \code{reshape2:::melt}, we melt \code{data.frame}s for the
#' special case in which we have multiple id variables and a single
#' value variable, and \code{matrix}s. This function is built for speed.
#' 
#' If items to be stacked are not of the same internal type, they will be
#' promoted in the order \code{logical} > \code{integer} > \code{numeric} > \code{character}.
#' 
#' @param data The \code{data.frame} to melt.
#' @param id.vars Vector of id variables. Can be integer (variable index) or
#' string (variable name). All variables not included here are assumed
#' stackable, and will be coerced as needed.
#' @examples
#' n <- 20
#' tmp <- data.frame( stringsAsFactors=FALSE,
#'   x=sample(letters, n, TRUE), 
#'   y=sample(LETTERS, n, TRUE),
#'   za=rnorm(n), 
#'   zb=rnorm(n), 
#'   zc=rnorm(n)
#' )
#'   
#' out2 <- melt_(tmp, c("x", "y"))
#' @export
melt_ <- function(data, id.vars) {
  UseMethod("melt_")
}

#' @rdname melt_
#' @method melt_ data.frame
#' @S3method melt_ data.frame
melt_.data.frame <- function(data, id.vars) {
  
  if( is.character(id.vars) ) {
    measure.vars <- which( names(data) %nin% id.vars )
  } else {
    measure.vars <- which( 1:length(data) %nin% id.vars )
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
  
  return( .Call("melt_dataframe",
                data[id.vars],
                data[measure.vars],
                PACKAGE="Kmisc"
  ) )
  
}

#' @rdname melt_
#' @method melt_ matrix
#' @S3method melt_ matrix
melt_.matrix <- function( data, id.vars ) {
  if( !missing(id.vars) ) {
    warning("data is a matrix; id.vars argument ignored")
  }
  return( .Call("melt_matrix", data, PACKAGE="Kmisc") )
}
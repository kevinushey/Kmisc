#' Melt a Data Frame
#' 
#' Inspired by \code{reshape2:::melt.data.frame}, this function will melt a 
#' \code{data.frame} for the special case in which we have \code{n}
#' identification variables but only 1 value variable. This function
#' is built for speed and can melt large \code{data.frame}s faster than
#' \code{reshape2:::melt}.
#' 
#' If items to be stacked are not of the same internal type, they will be
#' promoted in the order \code{logical} -> \code{integer} -> \code{numeric} -> \code{character}.
#' 
#' @param data The \code{data.frame} to melt.
#' @param id.vars Vector of id variables. Can be integer (variable index) or
#' string (variable name). All variables not included here are assumed
#' stackable.
#' @export
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
melt_ <- function(data, id.vars) {
  
  if( is.character(id.vars) ) {
    measure.vars <- which( names(data) %nin% id.vars )
  } else {
    measure.vars <- which( 1:length(data) %nin% id.vars )
  }
  
  ## coerce factors to characters
  if( any( sapply( data, is.factor ) ) ) {
    warning("factors coerced to characters")
    data <- factor_to_char(data)
  }
  
  ## check and coerce the types of the measure.vars
  types <- sapply( data[measure.vars], typeof )
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
  
  out <- .Call("melt_dataframe",
               data[id.vars],
               data[measure.vars],
               nrow(data),
               PACKAGE="Kmisc"
               )
  attr(out, "class") <- "data.frame"
  attr(out, "row.names") <- 1:length( out[[1]] )
  attr(out, "names") <- c( names(data[id.vars]), c("names", "value") )
  
  return(out)
  
}
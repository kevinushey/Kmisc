##' Display the Anatomy of a Data Frame
##' 
##' This function displays the 'anatomy' of a data frame. In practice, it's used
##' to implement a faster version of \code{str} for data frames built entirely
##' of atomic vectors, as \code{str.data.frame} is very slow for large data 
##' frames. If there are non-atomic vectors in \code{df}, we fall back to
##' \code{base::str}.
##' 
##' @param df An object inheriting class \code{data.frame}.
##' @param n The number of elements to print from each vector.
##' @param cols The number of columns to print from the \code{data.frame}.
##' @export
##' @examples
##' \dontrun{ 
##' local({
##'   bigDF <- as.data.frame( matrix( factor(1:1E3), nrow=1E3, ncol=1E3 ) )
##'   sink( tmp <- tempfile() )
##'   str <- system.time( str(bigDF, list.len=1E3) )
##'   anat <- system.time( anat(bigDF) )
##'   sink()
##'   unlink(tmp)
##'   print( rbind( str, anat ) )
##' }) 
##' }
anat <- function(df, n=3, cols=99) {
  
  if (!is.data.frame(df)) {
    warning("object is not a data.frame; returning utils::str(df)")
    cat("\n")
    return( utils::str(df) )
  } else {
    if( any( sapply( seq_along(df), function(i) { !is.atomic(df[[i]]) } ) ) ) {
      warning("object is a data.frame, but there are non-atomic columns; returning utils::str(df)")
      cat("\n")
      return( utils::str(df) )
    }
  }
  
  ## handle extra classes for df object
  class_statement <- paste( sep="", "'", class(df), "'", collapse=", " )
  
  if( all( dim( df ) == c(0, 0) ) ) {
    cat( paste( sep="", 
      class_statement, " with 0 rows and 0 columns\n" ) )
    return( invisible(NULL) )
  }
  
  if( nrow(df) < n ) {
    n <- nrow(df)
  }
  
  ## make formatted column names
  formatted_names <- format( names(df) )
  classes <- sapply( 1:ncol(df), function(i) {
    class( df[[i]] )
  })
  
  ## make the classes smaller
  class_replace <- c("logical", "integer", "numeric", "complex", "character", "raw", "list", "factor")
  names(class_replace) <- c("logi", "int", "num", "cplx", "chr", "raw", "list", "Factor")
  classes <- swap( classes, class_replace )
  
  ## handle factor levels
  levs <- sapply( 1:ncol(df), function(i) {
    if( is.factor(df[[i]]) ) {
      return( nlevels(df[[i]]) )
    } else {
      return( NA )
    }
  })
  
  levs_statement <- ifelse( levs[!is.na(levs)] > 1, " levels", " level" )
  levs_dots <- ifelse( levs[!is.na(levs)] > n, ", ... ", "" )
  classes[ !is.na(levs) ] <- paste( sep="",
    classes[ !is.na(levs) ], " w/ ", levs[!is.na(levs)], levs_statement
  )
  
  formatted_classes <- format( classes )
  
  cat( paste( sep="", 
    class_statement, " with ",
    nrow(df), " row", if( nrow(df) > 1 || nrow(df) == 0 ) "s", 
    " and ", ncol(df), " column", if( ncol(df) > 1 || nrow(df) == 0 ) "s", 
    ":\n" ) )
  
  ## N is the maximum number of columns to go through
  N <- min(length(df), cols)
  
  for (i in seq_len(N)) {
    
    var <- df[[i]][1:n]
    
    if (is.character(var)) {
      tmp <- paste( sep="", '"', var, '"' )
    } else if( is.factor(var) ) {
      tmp <- paste( sep="", '"', as.character(var), '"' )
    } else if( is.numeric(var) ) {
      tmp <- prettyNum( round(var, 3) )
    } else {
      tmp <- var
    }
    
    tmp[ is.na(var) ] <- NA
    formatted_vec <- paste( sep="", collapse=", ", tmp )
    if (is.factor(var)) {
      formatted_vec <- paste( sep="", formatted_vec, ": ", 
        paste( collapse=", ", as.integer(var) )
      )
    }
    
    out <- paste( sep="", collapse="",
      "  $ ", formatted_names[i], ": ",
      formatted_classes[i], "  ",
      formatted_vec,
      if( nrow(df) > n ) { ", ... " },
      "\n"
    )
    
    cat( format(out) )
    
  }
  
  if (length(df) > cols) {
    cat("\t\t[list output truncated]\n")
  }
  
  ## cat out the non-standard attributes
  attrs <- attributes(df)
  attrs <- attrs[ names(attrs) %nin% c("names", "class", "row.names") ]
  for (i in seq_along(attrs)) {
    cat("- attr(*, \"", names(attrs)[i], "\")=", 
      capture.output(utils::str(attrs[[i]])), 
      "\n", sep="")
  }
  
  return(invisible(NULL))
  
}

##' @rdname anat
##' @export
anatomy <- anat

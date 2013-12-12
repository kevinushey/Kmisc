##' Number of non-NA unique elements in a vector
##'  
##' Returns the number of non-NA unique elements in a vector. A wrapper to
##' \code{length( unique( x[!is.na(x)], ... ) )}. 
##' Primarily intended for interactive, not programmatic, use.
##' @export
##' @param x a vector
##' @param ... passed to \code{\link{unique}}
lu <- function( x, ...) {
  length( unique( x[!is.na(x)], ... ) ) 
}

##' Unique elements in a vector
##' 
##' Returns the unique elements in a vector. A wrapper to
##' \code{\link{unique}()}.
##' Primarily intended for interactive, not programmatic, use.
##' @param ... passed to \code{\link{unique}}.
u <- function(...) { 
  unique( ... ) 
}

##' length( grep( ... ) )
##' 
##' This is a wrapper to a \code{length( grep( ... ) )}. See examples for usage.
##' Primarily intended for interactive, not programmatic, use.
##' @param pattern regex pattern passed to \code{grep}.
##' @param x a vector on which we attempt to match \code{pattern} on.
##' @param perl boolean. use perl-compatible regular expressions?
##' @param ... additional arguments passed to \code{\link{grep}}.
##' @seealso \code{\link{re_exists}}
##' @export
##' @examples
##' x <- c("apple", "banana", "cherry")
##' if( lg( "^ap", x ) > 0 ) {
##'   print( "regular expression '^ap' found in 'x'" )
##'   }
lg <- function(pattern, x, perl=TRUE, ...) {
  length( grep( pattern, x, perl=perl, ... ) ) 
}

##' unlist( strsplit( ... ) )
##' 
##' This is a thin wrapper to \code{ unlist( strsplit( ... ) ) }.
##' Primarily intended for interactive, not programmatic, use.
##' @param x vector of items, as passed to \code{\link{strsplit}}
##' @param split the delimiter to split on
##' @param ... optional arguments passed to strsplit
##' @export
##' @seealso \code{\link{unlist}}, \code{\link{strsplit}}
##' @examples
##' x <- "apple_banana_cherry"
##' us(x, "_")
us <- function(x, split="", ...) { unlist( strsplit( x, split=split, ...) ) }

##' Set Working Directory
##' 
##' A small convenience function that wraps \code{file.path} into a
##' \code{setwd} call.
##' 
##' @param ... the set of strings to paste together. if no arguments are 
##' submitted, then we return to the home directory.
##' @export
##' @examples
##' x <- "my_favourite_dir"
##' #setwd( "C:/", x, "really_awesome_stuff" )
##' ## calls setwd( paste( "C:/", x, "really_awesome_stuff", collapse="" ) )
cd <- function(...) {
  args <- list(...)
  if( length(args) == 0 ) {
    base::setwd("~")
    return( invisible(NULL) )
  }
  base::setwd( file.path( ... ) )
  return( invisible(NULL) )
}

##' Apply a Function over a List
##' 
##' A convenience function that works like \code{lapply}, but coerces the output
##' to a \code{data.frame} if possible. We set \code{stringsAsFactors=FALSE}, and
##' \code{optional=TRUE}, to minimize the amount of automatic coersion \R might
##' try to do.
##' 
##' This function is preferable to \code{\link{sapply}} or \code{\link{lapply}}
##' when you explicitly want a data frame returned.
##' 
##' @param X a vector, expression object, or a \code{data.frame}
##' @param FUN function to be applied to each element of \code{X}.
##' @param ... optional arguments to \code{FUN}.
##' @seealso \code{\link{lapply}}, \code{\link{lapply}}
##' @export
##' @importFrom data.table setattr
##' @examples
##' dat <- data.frame(
##'   x = rnorm(10),
##'   y = rnorm(10)
##' )
##'   
##' ## Calculate 0.025, 0.975 quantiles for each column in a data.frame,
##' ## and return result as data.frame .
##' dapply( dat, function(x) { 
##'   quantile(x, c(0.025, 0.975))
##' } )
##'   
##' dapply( dat, summary )
##' str( dapply( dat, summary ) )
dapply <- function(X, FUN, ...) {
  tmp <- lapply(X, FUN, ...)
  nm <- names(tmp[[1]])
  list2df(tmp, inplace=TRUE)
  if (!is.null(nm)) {
    setattr(tmp, "row.names", nm)
  }
  return(tmp)
}

##' Read Tabular Data from the Clipboard
##' 
##' Convenience function for reading tabular data from the clipboard. 
##' The function checks the system OS and provides the appropriate wrapper 
##' call to \code{\link{read.table}}.
##' @export
##' @param sep the delimiter used in the copied text.
##' @param header boolean; does the first row contain column names?
##' @param ... optional arguments passed to \code{read.table}.
##' @seealso \code{\link{read.table}}
##' @examples
##' ## with some data on the clipboard, simply write
##' # x <- read.cb()
read.cb <- function(sep='\t', header=TRUE, ...) {
  sn <- Sys.info()["sysname"]
  if (sn == "Darwin") {
    read.table( pipe("pbpaste"), header=header, sep=sep, ... )
  } else if (sn == "Windows") {
    read.table( "clipboard", header=header, sep=sep, ... )
  } else {
    stop("Reading from the clipboard is not implemented for your system (", 
      sn, ") in this package.")
  }
}

##' Read Data from the Clipboard
##' 
##' Convenience function for reading data from the clipboard.
##' Wraps to \code{\link{scan}}. By default, we assume the data is 
##' \code{character}, and delimit by new lines.
##' @export
##' @param what passed to \code{scan}.
##' @param sep passed to \code{scan}.
##' @param quiet passed to \code{scan}.
##' @param ... passed to \code{scan}.
##' @seealso \code{\link{scan}}
scan.cb <- function( what=character(), sep="\n", quiet=TRUE, ... ) {
  sn <- Sys.info()["sysname"]
  if (sn == "Darwin") {
    file <- pipe("pbpaste")
    output <- scan( file, what=what, sep=sep, quiet=quiet, ... )
    close(file)
  } else if (sn == "Windows") {
    output <- scan( "clipboard", what=what, sep=sep, quiet=quiet, ... )
  } else {
    stop("Reading from the clipboard is not implemented for your system (", 
      sn, ") in this package.")
  }
  return(output)
}

##' Write Tabular Data to the Clipboard
##' 
##' Directs output of \code{write.table} to the clipboard. This can be
##' useful if you want to quickly write some \R table out and paste it into
##' some other file, eg. a Word document, Excel table, and so on.
##' @export
##' @param dat the data file you want to write out; passed to \code{write.table}.
##' @param row.names logical. include the row names of dat?
##' @param col.names logical. include the column names of dat?
##' @param sep the delimiter used to separate elements after exporting dat.
##' @param quote logical. include quotes around character vectors in dat?
##' @seealso \code{\link{write.table}}
##' 
write.cb <- function( dat, 
  row.names=FALSE, 
  col.names=TRUE, 
  sep='\t', 
  quote=FALSE ) {
  
  sn <- Sys.info()["sysname"]
  
  if (sn == "Darwin") {
    file <- pipe("pbcopy")
    write.table( dat, file=file,
      row.names=row.names,
      col.names=col.names,
      sep=sep,
      quote=quote
    )
    close(file)
  } else if (sn == "Windows") {
    write.table( dat, file="clipboard",
      row.names = row.names,
      col.names = col.names,
      sep = sep,
      quote = quote
    )
  } else {
    stop("Writing to the clipboard is not implemented for your system (", 
      sn, ") in this package.")
  }
  
}

##' Write Data to the Clipboard
##' 
##' This function writes data to the clipboard, using \code{\link{cat}}.
##' @param dat data to be written to the clipboard.
##' @param ... optional arguments passed to \code{cat}.
##' @seealso \code{\link{write.cb}}, \code{\link{cat}}
##' @export
cat.cb <- function( dat, ... ) {
  sn <- Sys.info()["sysname"]
  if (sn == "Darwin") {
    file <- pipe("pbcopy")
    cat( dat, file=file, ... )
    close(file)
  } else if (sn == "Windows") {
    cat( dat, file="clipboard", ... )
  } else {
    stop("Reading from the clipboard is not implemented for your system (", 
      sn, ") in this package.")
  }
}

##' Converts Factors to Characters in an Object
##' 
##' Converts factors to characters in an object. Leaves non-character
##' elements untouched.
##' 
##' We iterate through all elements in the object (e.g. if
##' it is a list) and convert anything that is a factor into a character.
##' @param X an object.
##' @param inplace Boolean; if \code{TRUE} we modify the object in place.
##'   Useful if you're modifying a list and don't want to force a copy, but
##'   be wary of other symbols pointing as the same data.
##' @export
factor_to_char <- function(X, inplace=FALSE) {
  invisible( .Call(Cfactor_to_char, X, as.logical(inplace)) )
}

##' Converts Characters to Factors in an Object
##' 
##' Converts characters to factors in an object. Leaves non-factor elements
##' untouched.
##' 
##' @param X an object.
##' @param inplace boolean; if \code{TRUE} the object is modified in place.
##'   Useful if you're modifying a list and don't want to force a copy, but
##'   be wary of other symbols pointing as the same data.
##' @param ... Ignored.
##' @export
char_to_factor <- function(X, inplace=FALSE, ...) {
  invisible( .Call(CKmisc_char_to_factor, X, inplace) )
}

##' Make Dummy Variables from a Factor
##' 
##' This functions converts a single factor into dummy variables, with one
##' dummy variable for each level of that factor. Names are constructed as
##' \code{<varName>_<level>}.
##' @param x an object coercable to \code{factor}.
##' @export
##' @examples
##' x <- factor( rep( c("a", "b", "c", "d"), each=25 ) )
##' make_dummy(x)
make_dummy <- function(x) {
  
  if( is.data.frame(x) ) {
    name <- names(x)
    x <- x[,1]
  } else {
    name <- deparse( substitute( x ) )
    name <- gsub( ".*\\$", "", name )
  }
  
  levs <- unique(x)  
  out <- vector("list", length(levs))
  for( i in seq_along(levs) ) {
    lev <- levs[i]
    tmp <- as.character(x)
    tmp[ x == lev ] <- 1
    tmp[ x != lev ] <- 0
    out[[lev]] <- tmp
    names( out )[i] <- paste( name, lev, sep = "_" )
  }
  
  return( char_to_factor( as.data.frame(do.call(cbind, out)) ) )
  
}

##' k-means Diagnostic Plot
##' 
##' Using \code{kmeans}, plot percentage variance explained vs. number of clusters.
##' Used as a means of picking \code{k}.
##' 
##' @importFrom lattice xyplot panel.grid panel.polygon panel.xyplot panel.abline
##' @param dat numeric matrix of data, or an object that can be coerced to 
##' such a matrix (such as a numeric vector or a data frame with all 
##' numeric columns).
##' @param nmax maximum number of clusters to examine
##' @param ... optional arguments passed to xyplot
##' @seealso \code{\link{kmeans}}
##' @export
##' @examples
##' data(iris)
##' kmeans_plot(iris[,1:4])
kmeans_plot <- function( dat, nmax=20, ... ) {
  y <- rep(0,nmax)
  for( i in 1:nmax ) {
    tmp <- kmeans( dat, i )
    y[i] <- tmp$betweenss / tmp$totss
  }
  print( xyplot( y ~ (1:nmax), type = c("p", "l"), ... ) )
}

##' Strip File Extension
##' 
##' Strips the extension from a file name. By default, we assume the extension 
##' is separated from the file name by a single period; however, the \code{lvl} 
##' argument lets us specify how many periods are used in forming the file 
##' extension.
##' @param x the file name, including extension.
##' @param lvl the number of \code{'.'} used in defining the file extension.
##' @export
##' @examples
##' x <- "path_to_file.tar.gz"
##' strip_extension(x, lvl=2)
strip_extension <- function(x, lvl=1) {
  if( length(x) == 0 ) { 
    return(x)
  }
  tmp <- unlist( strsplit( x, "/" ) )
  tmp2 <- tmp[length(tmp)]
  tmp2 <- unlist( strsplit( tmp2, "\\." ) )
  tmp2 <- paste( tmp2[1:(length(tmp2)-lvl)], collapse ="." )
  tmp <- paste( tmp[-length(tmp)], collapse="/")
  if( tmp != "" ) {
    tmp2 <- paste( tmp, tmp2, sep = "/" )
  }
  return(tmp2)
}

##' Load and Assign an R Object
##' 
##' The regular \code{load} function keeps the old variable name used when 
##' saving that object. Often, we would prefer to assign the \code{load}ed 
##' object to a new variable name. Hence, this function.
##' 
##' If multiple arguments are supplied, they will be concatenated through
##' \code{file.path}.
##' @param ... args to pass to \code{load}
##' @seealso \code{\link{load}}
##' @export
##' @return the object stored in the \code{load}-ed object
##' @examples
##' dat <- data.frame( x = c(1,2,3), y=c('a','b','c') )
##' save( dat, file="dat.rda" )
##' rm( dat )
##' my_data <- kLoad( "dat.rda" ) 
##' ## we protect ourselves from 'forgetting' the name of the
##' ## object we saved
kLoad <- function( ... ) {
  get( load( file.path( ... ) ) )
}

##' @rdname kLoad
##' @export
getload <- kLoad

##' Write out and Save a Tabular File
##' 
##' A function that both writes a file to table with \code{write.table},
##' and saves it with the same name but a separate file extension.
##' @param x the R object you want to save / write to file
##' @param file the location to write the file to, with extension desired for object written by write.table
##' @param lvl how many extensions do you want to strip from your output file?
##' @param Rext the extension to use for the saved object.
##' @param ... optional arguments passed to \code{write.table}
##' @export
##' @examples
##' dat <- data.frame( x=c(1,2,3), y=c('a','b','c') )
##' kSave( dat, file="dat.txt" )
##' ## the file 'dat.rda' is written as well - let's see if it exists
##' dat2 <- kLoad( "dat.rda" )
##' stopifnot( identical(dat, dat2) ) ## TRUE
kSave <- function( x, file, lvl=1, Rext=".rda", ... ) {
  
  write.table( x,
    file = file,
    ... )
  
  tmp <- strip_extension( file, lvl=lvl )
  
  save( x,
    file=paste( tmp, Rext, sep="" )
  )
  
}

##' Merge (Left Join) with Order Retainment
##' 
##' \code{merge} will mangle the order of the data frames it is merging. This is
##' a simple modification to ensure that the order in data frame \code{x} is preserved
##' when doing a 'left join'; ie, \code{merge( x, y, all.x=TRUE, ... )}.
##' That is, if we want to merge a data frame \code{x} with another 
##' data frame \code{y}, we can merge in the parts of \code{y} whose index matches
##' with that of \code{x}, while preserving the ordering of \code{x}.
##' 
##' @param x the \code{data.frame} you wish to merge \code{y} into.
##' @param y the \code{data.frame} to be merged.
##' @param by specifications of the columns used for merging. See 'Details' of \code{\link{merge}}.
##' @param by.x specifications of the columns used for merging. See 'Details' of \code{\link{merge}}.
##' @param by.y specifications of the columns used for merging. See 'Details' of \code{\link{merge}}.
##' @param ... optional arguments passed to \code{merge}.
##' @export
##' @return \code{data.frame}
##' @seealso \code{\link{merge}}
##' @examples
##' x <- data.frame( id=5:1, nums=rnorm(5) )
##' y <- data.frame( id=1:3, labels=c(1, 2, 2) )
##' merge(x, y, all.x=TRUE) ## re-ordered the data.frame
##' merge(x, y, all.x=TRUE, sort=FALSE) ## nope - NAs cause problems
##' kMerge(x, y, by="id") ## preserves ordering of x, even with NAs
##' 
##' ## an id entry appears more than once in y
##' y <- data.frame( id=c(1, 1, 2), labels=c(1, 2, 3) )
##' kMerge(x, y, by="id")
kMerge <- function( x, y, by=intersect( names(x), names(y) ), by.x=by, by.y=by, ... ) {
  
  x[["__KMERGE_TMP__"]] <- 1:nrow(x)
  tmp <- merge( x=x, y=y, by=by, by.x=by.x, by.y=by.y, all.x = TRUE, ... )
  tmp <- tmp[order(tmp[["__KMERGE_TMP__"]]),]
  tmp <- tmp[ names(tmp) !=  "__KMERGE_TMP__"]
  rownames(tmp) <- 1:nrow(tmp)
  return( tmp )
  
}

##' Generate Gradient from Continuous Variable
##' 
##' Assign colors based on a continuous variable. Useful for plotting functions
##' where you would like to generate a gradient based on (a function of) the
##' continuous variables you are plotting quickly.
##' 
##' @param x a continuous variable to generate colors over.
##' @param m the number of distinct colors you wish to pull from the pallete.
##' @param cols the colors to interpolate over. passed to \code{\link{colorRampPalette}}.
##' @seealso \code{\link{colorRampPalette}}
##' @export
##' @examples
##' dat <- data.frame(y=rnorm(100), x=rnorm(100))
##' with( dat, plot( y ~ x, col=gradient(y) ) )
gradient <- function(x, m=10, cols=c("darkorange","grey60","darkblue") ) {
  x.cut <- as.integer( cut(x, m) )
  cols <- colorRampPalette( cols )(m)
  x.cols <- cols[x.cut]
  return( x.cols )
}

##' Make Nicely Formatted Tables
##' 
##' Function for creating nice 1D and 2D tables. Tables are generated and 
##' formatted with both counts and percentages. Primarily intended to be used 
##' with R Markdown documents, calling some of the table printing functions. 
##' The function returns a \code{data.frame} in a format that can be used with
##' utility HTML generation functions.
##' @param x the \code{x} variable to build a table on.
##' @param y optional: the \code{y} variable to build a table on. 
##' Used for 2x2 contingency tables.
##' @param deparse.level passed to \code{table}; \code{deparse.level=2} allows 
##' us to pass through variable names.
##' @param top.left.cell the string to set in the top left cell of the table.
##' @param col.names a vector of column names to use on the outputted table; 
##' typically this is parsed from the variables passed through.
##' @param row.names a vector of row names to use on the outputted table; 
##' typically this is parsed from the variables passed through.
##' @param left.label the label to use for the rows; typically parsed from 
##' \code{x}. Only used for 2D tables (ie, when \code{y} is not null).
##' @param top.label the label to use for the columns; typically parsed from 
##' \code{y}. Only used for 2D tables (ie, when \code{y} is not null).
##' @param google used if you plan on passing the table to \code{gvistable} 
##' from the \code{googleVis} package.
##' @export
##' @examples
##' x <- rbinom(100, size=2, p=0.1)
##' y <- rbinom(100, size=2, p=0.1)
##' 
##' ## try these in an R markdown document for best results
##' kTable(x)
##' my_table <- kTable(x, y, top.left.cell="foo", left.label="bar", top.label="baz")
##' pxt( my_table )
kTable <- function( x, 
  y=NULL,
  deparse.level=2,
  top.left.cell="",
  col.names=NULL,
  row.names=NULL,
  left.label=NULL,
  top.label=NULL,
  google=FALSE 
) {
  
  if( !is.null(y) & isTRUE(google) ) {
    stop("cannot generate google tables from 2x2 contingency tables")
  }
  
  if( is.null(left.label) ) {
    left.label <- gsub( ".*\\$", "", deparse( substitute( x ) ) )
  }
  
  if( is.null( top.label ) ) {
    top.label <- gsub( ".*\\$", "", deparse( substitute( y ) ) )
  }
  
  suppressWarnings(
    if( any( is.na( x ) ) || any( is.na( y ) ) ) {
      useNA <- 'always'
    } else {
      useNA <- 'no'
    }
  )
  
  if( is.null(y) ) {
    
    tmp <- base::table( x, deparse.level=deparse.level, useNA=useNA )
    
    ## Row names
    if( is.null( row.names ) ) {
      my_rowNames <- rownames(tmp)
    } else {
      my_rowNames <- row.names
    }
    
    ## Col names
    if( is.null( col.names ) ) {
      my_colNames <- deparse( substitute( x ) )
    } else {
      my_colNames <- col.names
    }
    
    ## Row, column sums
    tmp <- matrix(tmp, nrow=nrow(tmp), ncol=1 )
    my_colSum <- colSums(tmp)
    my_rowSum <- rowSums(tmp)
    
    my_col_nchar <- apply( tmp, 2, function(x) { max( nchar( x ) ) } )
    
    ## Add in percentages
    for( i in 1:nrow(tmp) ) {
      
      for( j in 1:ncol(tmp) ) {
        
        tmp[i,j] <- paste( sep="",
          paste( sep="",
            rep(" ", times=my_col_nchar[j] - nchar(tmp[i,j])), collapse="" 
          ),
          tmp[i,j], 
          " (",
          substr( 
            sprintf("%.3f%%", as.numeric(tmp[i,j]) / my_colSum[j] * 100 ),
            1,
            4
          ),
          "%)"
          
        )
      }
    }
    
    tmp <- rbind( tmp, my_colSum )
    
    ## Row, column names
    tmp <- cbind( c( my_rowNames, "Total" ), tmp )
    tmp <- rbind( c( 
      gsub( ".*\\$", "", deparse( substitute( x ) ) ), 
      "Count (%)" 
    ), tmp )
    
    ## Top-Left cell
    if( top.left.cell != "" ) { 
      tmp[1,1] <- top.left.cell 
    } else {
      tmp[1,1] <- left.label
    }
    
  } else {
    
    tmp <- base::table( x, y, deparse.level=deparse.level, useNA=useNA )
    my_rowNames <- rownames(tmp)
    my_colNames <- colnames(tmp)
    
    ## Row, column sums
    tmp <- matrix(tmp, nrow=nrow(tmp) )
    my_rowSum <- rowSums(tmp)
    my_colSum <- colSums(tmp)
    
    my_col_nchar <- apply( tmp, 2, function(x) { max( nchar( x ) ) } )
    
    ## Add in percentages
    for( i in 1:nrow(tmp) ) {
      for( j in 1:ncol(tmp) ) {
        if( my_colSum[j] != 0 ) {
          
          tmp[i,j] <- paste( sep="",
            paste( sep="",
              rep(" ", times=my_col_nchar[j] - nchar(tmp[i,j])), collapse="" ),
            tmp[i,j], 
            " (",
            substr( 
              sprintf("%.3f%%", as.numeric(tmp[i,j]) / my_colSum[j] * 100 ),
              1,
              4
            ),
            "%)"
            
          )
          
        }
      }
    }
    
    ## Add in the row, column sums
    tmp <- cbind( tmp, my_rowSum )
    tmp <- rbind( tmp, c( my_colSum, sum(my_rowSum) ) )
    
    ## Row names
    tmp <- cbind( c( my_rowNames, "Total" ), tmp )
    
    ## Col names
    tmp <- rbind( c("", my_colNames, "Total" ), tmp )
    
    ## left Label
    tmp <- cbind( c("", left.label, rep("", times=nrow(tmp)-2 ) ), tmp )
    
    ## top label
    tmp <- rbind( c("", "", top.label, rep("", times=ncol(tmp)-3 ) ), tmp )
    
    ## top left cell
    if( is.null( top.left.cell ) ) {
      tmp[1,1] <- paste( 
        gsub( ".*\\$", "", deparse( substitute( x ) ) ),
        gsub( ".*\\$", "", deparse( substitute( y ) ) ),
        sep=" \\ "
      )
    } else {
      tmp[1,1] <- top.left.cell
    }
    
  }
  
  tmp[ is.na(tmp) ] <- "Missing"
  tmp <- gsub( "100\\.%", " 100%", tmp )
  
  ## Formatting for gvisTable
  if( google ) {
    rownames(tmp) <- 1:nrow(tmp)
    tmp <- as.data.frame( tmp, stringsAsFactors=FALSE )
    names(tmp) <- c(t(tmp[1,]))
    tmp <- tmp[-1,]
  }
  
  return(tmp)
  
}

##' Nicely Formatted Model Coefficient Output
##' 
##' A customized coefficient function that assigns better row names to the
##' coefficient matrix returned by \code{\link{coef}}() for a model fit. Also
##' includes some arguments for parsing of variable names. 
##' 
##' NOTE: 
##' 
##' Models with interaction effects are currently not handled.
##' 
##' @param fit the model fit we wish to generate coefficients from.
##' @param remove_underscore remove underscores (and all elements after) in a variable?
##' @param remove_dollar remove all elements before and including a $ in a variable name?
##' @param swap_periods swap periods with spaces?
##' @note The names given assume default contrasts in your model fit; ie,
##' the default is \code{contr.treatment}, where each level of a factor is 
##' compared to a reference.
##' @export
##' @return a matrix of coefficients with nicely formatted names.
##' @examples
##' 
##' ## How the remove_underscore and remove_dollar arguments act:
##' ## An example:
##' ##                     kDat$variable_other_stuff
##' ## remove_underscore:  +++++++++++++------------
##' ## remove_dollar:      -----++++++++++++++++++++
##' 
##' x <- rnorm(100); y <- x * runif(100)
##' z <- as.factor( rep( c("apple", "banana", "cherry", "date"), each=25 ) )
##' myFit <- lm( y ~ x + z )
##' 
##' ## compare the output of these two: which do you prefer?
##' coef( summary( myFit ) )
##' kCoef( myFit )
kCoef <- function( fit, 
  remove_underscore=TRUE, 
  remove_dollar=TRUE,
  swap_periods=TRUE
) {
  
  if( re_exists(names(fit$coefficients), ":") ) {
    warning("kCoef not implemented for models with interaction effects")
  }
  
  dat <- fit$data
  coef_matrix <- coef( summary( fit ) )
  
  ## get the factors used in the model fit
  my_factors <- fit$model[sapply( fit$model, is.factor )]
  
  ## if there are no factors, just return the matrix
  if( length(my_factors) == 0 ) {
    if( remove_underscore ) {
      rownames( coef_matrix ) <- gsub( "_.*", "", rownames(coef_matrix) )
    }
    if( remove_dollar ) {
      rownames( coef_matrix ) <- gsub( ".*\\$", "", rownames(coef_matrix) )
    }
    
    if( swap_periods ) {
      rownames( coef_matrix ) <- gsub( "\\.", " ", rownames(coef_matrix) )
    }
    
    return( coef_matrix )
  }
  
  ## drop the first column if it was the dependent variable
  if( all(fit$model[,1] == my_factors[,1]) ) {
    my_factors <- my_factors[,-1,drop=FALSE]
  }
  
  ## if there are no factors, just return the matrix
  if( length(my_factors) == 0 ) {
    if( remove_underscore ) {
      rownames( coef_matrix ) <- gsub( "_.*", "", rownames(coef_matrix) )
    }
    if( remove_dollar ) {
      rownames( coef_matrix ) <- gsub( ".*\\$", "", rownames(coef_matrix) )
    }
    if( swap_periods ) {
      rownames( coef_matrix ) <- gsub( "\\.", " ", rownames(coef_matrix) )
    }
    
    return( coef_matrix )
  }
  
  my_factor_levels <- lapply( my_factors, levels )
  
  ## generate old factor names
  old_factor_names <- NULL
  for( i in 1:length(my_factor_levels) ) {
    cFactor <- names(my_factor_levels)[i]
    for( j in 2:length(my_factor_levels[[i]]) ) {
      cName <- my_factor_levels[[i]][j]
      old_factor_names <- c( old_factor_names, paste( cFactor, cName, sep="" ))
    }
  }
  
  ## generate new factor names
  new_factor_names <- NULL
  for( i in 1:length(my_factor_levels) ) {
    cFactor <- names(my_factor_levels)[i]
    if( remove_underscore ) {
      cFactor <- gsub( "_.*", "", cFactor )
    }
    cRef <- my_factor_levels[[i]][1]
    for( j in 2:length(my_factor_levels[[i]]) ) {
      cName <- my_factor_levels[[i]][j]
      new_factor_names <- c( new_factor_names, 
        paste( cFactor, ": ", cRef, " -> ", cName, sep="" )
      )
    }
  }
  
  ## handle interaction effects
  new_interactions <- grep( ":", colnames( attr( fit$terms, "factors" ) ), value=T )
  old_interactions <- grep( ":", rownames( coef_matrix ), value=T )
  
  
  ## swap the row names
  rownames( coef_matrix ) <- swap( rownames( coef_matrix),
    old_factor_names,
    new_factor_names
  )
  
  rownames( coef_matrix ) <- swap( rownames( coef_matrix),
    old_interactions,
    new_interactions
  )
  
  ## remove underscore stuff?
  if( remove_underscore ) {
    rownames( coef_matrix ) <- gsub( "_.*", "", rownames( coef_matrix ) )
  }
  
  ## remove stuff before a '$'?
  if( remove_dollar ) {
    rownames( coef_matrix ) <- gsub( ".*\\$", "", rownames( coef_matrix ) )
  }
  
  ## swap periods with a space?
  if( swap_periods ) {
    rownames( coef_matrix ) <- gsub( "\\.", " ", rownames(coef_matrix) )
  }
  
  return( coef_matrix )
  
}

##' Nicely Formatted ANOVA Table
##' 
##' Returns a nicely formatted ANOVA table. 
##' See \code{\link{kCoef}} for other details.
##' @param fit the model fit to generate an ANOVA table for.
##' @param test the type of test to perform. default is likelihood-ratio test (LRT).
##' @param swap.periods swap periods with spaces?
##' @export
##' @examples
##' x <- rnorm(100)
##' y <- ifelse( x + runif(100) > 1, 1, 0 )
##' myFit <- glm( y ~ x, family="binomial" )
##' kAnova( myFit )
kAnova <- function( fit, test="LRT", swap.periods=TRUE ) {
  
  f <- function(x) {
    tmp <- strsplit( x, ":" )
    tmp <- lapply( tmp, function(x) { gsub( ".*\\$", "", x ) } )
    tmp <- unlist( lapply( tmp, function(x) { paste(x, collapse=":" ) } ) )
    return( tmp )
  }
  
  tmp <- anova( fit, test="LRT" )
  class(tmp) <- "data.frame"
  rownames( tmp ) <- f( rownames(tmp) )
  rownames( tmp ) <- gsub( "_.*", "", rownames(tmp) )
  rownames( tmp )[ rownames(tmp) == "NULL" ] <- "Null Model"
  
  if( swap.periods ) {
    rownames( tmp ) <- gsub( "\\.", " ", rownames(tmp) )
  }  
  
  return( tmp )
  
}

##' Fivenum with Names
##' 
##' A wrapper to \code{stats::fivenum} that also produces variable names.
##' @param x numeric, maybe including \code{NA}s and \code{Inf}s.
##' @param na.rm logical. remove NAs?
##' @seealso \code{\link{fivenum}}
##' @export
##' @return \code{data.frame} version of five number summary
kFivenum <- function(x, na.rm=TRUE) {
  tmp <- as.data.frame( matrix( stats::fivenum(x, na.rm=na.rm), ncol=5 ) )
  names(tmp) <- c("Min", "1st Qu.", "Median", "3rd Qu.", "Max")
  tmp
}

##' Get all Objects in Environment
##' 
##' Get all objects within an environment. Useful for inspecting the objects
##' available in a particular environment.
##' @param env an environment.
##' @return a list of the objects contained within that environment.
##' @export
##' @examples
##' myenv <- new.env()
##' assign( "foo", "bar", env=myenv )
##' assign( "baz", "spam", env=myenv )
##' getObjects( myenv )
getObjects <- function( env ) {
  stopifnot( typeof( env ) == "environment" )
  objs <- lapply( objects(env), function(x) {
    get( x, envir=env )
  })
  names( objs ) <- objects(env)
  return(objs)
}

##' Remove NA Entries from a Vector
##' 
##' This function removes all \code{NA} entries from a vector.
##' 
##' For \code{data.frames}, we use \code{complete.cases} to remove \code{NA}s,
##' and hence remove all rows for which an \code{NA} value in encountered.
##' 
##' @param x An (atomic) vector, or a list / data.frame.
##' @export
remove_na <- function(x) {
  if( is.data.frame(x) ) {
    return( x[complete.cases(x), ] )
  } else if( is.list(x) ) {
    return( lapply(x, remove_na) )
  } else {
    return( x[ !is.na(x) ] )
  }
}

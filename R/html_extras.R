##' Make HTML Table from R 'table-like' Object
##' 
##' Function for making HTML tables from an \R 'table-like' object; ie, a
##' \code{data.frame} or a \code{matrix}. It simply parses the item as an HTML table.
##'
##' The row.spans and col.spans argument can be specified as a matrix to
##' set the row or column span of a certain cell to be >1, if desired.
##' See \code{\link{pxt}} for an example implementation.
##' It will also handle 'boxes', e.g. cells with both rowspan and colspan > 1.
##' 
##' Note that the default behavior is to \code{'clean'} numeric input; this prints
##' numeric values with a maximum of four digits; ie, through the \code{"\%.4G"} format
##' specifier. Alternatively,
##' you can use a format specifier (as used in \code{sprintf}) to ensure numbers
##' are formatted and displayed as desired.
##' 
##' @param x the \code{data.frame} / \code{matrix} you want to convert to an HTML table.
##' @param attr attributes to be passed to the \code{<table>} tag, as raw HTML.
##' @param row.spans a matrix specifying desired row.spans, for largers cells.
##' @param col.spans a matrix specifying desired column spans, for larger cells.
##' @param use.row.names if you submit an object with row names, use those names in 
##' construction of the table.
##' @param use.col.names if you submit an object with column names, use those names in
##' construction of the table.
##' @param clean boolean. if \code{TRUE}, we print all numeric values with 4 digits.
##' Alternatively, we can pass a format specifier as used by \code{\link{sprintf}}.
##' @param replace.periods replace periods with spaces?
##' @export
##' @examples
##' dat <- data.frame( apple=c(1.2150125, 2, 3), banana=c("a", "b", "c") )
##' makeHTMLTable( dat ) 
makeHTMLTable <- function( x, 
  attr,
  row.spans=0,
  col.spans=0,
  use.row.names=FALSE,
  use.col.names=FALSE,
  clean=TRUE,
  replace.periods=TRUE 
) {
  
  if (missing(attr))
    attr <- 'class="table table-collapsed table-hover table-striped"'
  
  if( length(x) == 0 ) {
    warning( paste(x, "is of length zero") )
    x <- ""
  }
  
  x <- as.data.frame(x, stringsAsFactors=FALSE, optional=TRUE )
  if( use.row.names == FALSE ) {
    rownames(x) <- NULL
  }
  
  if( use.col.names == FALSE ) {
    colnames(x) <- paste(sep="", "x_", 1:ncol(x) )
  }
  
  ## We round any numeric entries so that they aren't printed with
  ## an excessive number of decimal places.
  
  if( clean == TRUE || length( grep("^\\%", clean ) ) > 0 ) {
    if( clean == TRUE ) {
      clean <- "%.4G"
    }
    my_rownames <- rownames(x)
    x <- as.data.frame( lapply( x, function(xx) {
      if( class(xx) %in% c("numeric", "integer") ) {
        return( sprintf( clean, xx ) )
      } else {
        return( xx )
      }
    }), stringsAsFactors=FALSE, optional=TRUE )
    rownames(x) <- my_rownames
  }
  
  if( use.row.names == TRUE ) {
    x <- cbind( rownames(x), x )
  }
  
  if( use.col.names == TRUE ) {
    x <- factor_to_char( x )
    if( replace.periods ) {
      colnames(x) <- gsub( "\\.", " ", colnames(x) )
    }
    x <- rbind( colnames(x), x )
    
    if( use.row.names == TRUE ) {
      x[1,1] <- ""
    }
    
    
    ## Printing the basic table
    if( row.spans == 0 && col.spans == 0 ) {
      
    }
    out <- paste( sep="", "<table ", attr, ">" )
    out <- paste( sep="", out, "<thead><tr>" )
    for (j in 1:ncol(x)) {
      out <- paste( sep="", out, "<th>", x[1, j], "</td>" )
    }
    out <- paste( sep="", out, "</thead></tr>" )
    out <- paste( sep="", out, "<tbody>")
    for( i in 2:nrow(x) ) {
      out <- paste( sep="", out, "<tr>" )
      for( j in 1:ncol(x) ) {
        out <- paste( sep="", out, "<td>", x[i,j], "</td>" )
      }
      out <- paste( sep="", out, "</tr>" )
    }
    
    out <- paste( sep="", out, "</tbody></table>" )
    
    cat(out, "\n")
    
  } else {
    
    ## Handling tables with specialized row, col spans.
    
    ## First, verify that we're receiving acceptable matrices.
    if( !identical( row.spans, 0 ) ) {
      if( length( unique( apply( row.spans, 1, sum ) ) ) != 1 ) {
        stop("ERROR: Invalid row.spans matrix specified.")
      }
    } else {
      row.spans <- matrix(1, ncol=ncol(x), nrow=nrow(x) )
    }
    
    if( !identical( col.spans, 0 ) ) {
      if( length( unique( apply( col.spans, 2, sum ) ) ) != 1 ) {
        stop("ERROR: Invalid col.spans matrix specified.")
      }
    } else {
      col.spans <- matrix(1, ncol=ncol(x), nrow=nrow(x) )
    }
    
    ## Pre-process the table: if we see a number greater in a particular
    ## cell for both tables (implying 2x2 or greater cell block), then we set
    ## numbers in the lower-right cells to zero.
    
    for( i in 1:nrow(row.spans) ) {
      for( j in 1:ncol(row.spans) ) {
        if( row.spans[i,j] > 1 && col.spans[i,j] > 1 ) {
          
          cell_width <- row.spans[i,j]
          cell_height <- col.spans[i,j]
          
          row.spans[ i:(i+cell_width-1), j:(j+cell_height-1) ] <- 0
          col.spans[ i:(i+cell_width-1), j:(j+cell_height-1) ] <- 0
          
          row.spans[i,j] <- cell_width
          col.spans[i,j] <- cell_height
          
        }
      }
    }
    
    out <- paste( sep="", "<table ", attr, ">" )
    
    for( i in 1:nrow(x) ) {
      out <- paste( sep="", out, "<tr>" )
      for( j in 1:ncol(x) ) {
        
        cRowSpan <- row.spans[i,j]
        cColSpan <- col.spans[i,j]
        
        openTag <- "<td "
        
        ## only generate a td if both the elements in row, colspan are non-zero
        
        cond1 <- cRowSpan != 0 && cColSpan != 0
        
        if( cond1 ) {
          
          if( cRowSpan > 1 ) {
            openTag <- paste( sep="", openTag, "colspan=", cRowSpan, " " )
          }
          if( cColSpan > 1 ) {
            openTag <- paste( sep="", openTag, "rowspan=", cColSpan, " " )
          }
          
          openTag <- paste( sep="", openTag, ">" )
          out <- paste( sep="", out, openTag, x[i,j], "</td>" )
          
        }
        
      }
      
      out <- paste( sep="", out, "</tr>" )
      
    }
    
    out <- paste( sep="", out, "</table>" )
    
    cat(out, "\n")
    
  }
  
}

##' Make 1D HTML Table
##' 
##' This tabling function is intended for the output of \code{kTable}, as
##' generated when only one 'data' argument is passed.
##' @export
##' @param x a \code{data.frame}, typically output of \code{kTable}.
##' @param class class to be passed to HTML table; used for CSS styling.
##' @param id id to be passed to HTML table; used for CSS styling.
##' @param ... optional arguments passed to \code{\link{makeHTMLTable}}.
##' @seealso \code{\link{kTable}}, \code{\link{makeHTMLTable}}
##' @examples
##' y <- factor( rbinom( 100, 2, 0.2 ) )
##' p1t( kTable( y ) )
p1t <- function( x,
  class='table table-condensed table-striped table-hover',
  id=NULL,
  ... ) {
  
  warning("Deprecated: use knitr::kable instead.")
  
  myAttr <- NULL
  if( !is.null(class) ) myAttr <- paste( sep="", myAttr, "class='", class, "' ")
  if( !is.null(id) ) myAttr <- paste( sep="", myAttr, "id='", id, "'")
  
  makeHTMLTable( x, 
    attr=myAttr,
    clean=FALSE,
    ...
  )  
  
}

##' Make 2x2 HTML Contingency Table
##' 
##' Function for outputting cross-tabulated tables as marked-up HTML.
##' CSS styling can be used to make these tables look especially nice.
##' @param x a 2x2 table; typically something returned from \code{kTable(x,y)}
##' @param class class to be passed to HTML table; used for CSS styling.
##' @param id id to be passed to HTML table; used for CSS styling.
##' @param ... optional arguments passed to \code{\link{makeHTMLTable}}.
##' @export
##' @seealso \code{\link{kTable}}
##' @examples
##' x <- rbinom( 100, 2, 0.2 )
##' y <- rbinom( 100, 2, 0.2 )
##' pxt( kTable(x, y) )
pxt <- function( x, 
  class='twoDtable', 
  id=NULL, 
  ...) {
  
  myAttr <- NULL
  if( !is.null(class) ) myAttr <- paste( sep="", myAttr, "class='", class, "' ")
  if( !is.null(id) ) myAttr <- paste( sep="", myAttr, "id='", id, "'")
  
  my_row_span <- matrix(1, nrow=nrow(x)-1, ncol=ncol(x))
  my_row_span <- rbind( c( 2, 0, ncol(x)-3, rep(0, ncol(x)-4), 1), my_row_span )
  
  my_col_span <- matrix(1, nrow=nrow(x), ncol=ncol(x)-1)
  my_col_span <- cbind( c( 2, 0, nrow(x)-3, rep(0, nrow(x)-4), 1 ), my_col_span )
  
  makeHTMLTable( x, 
    attr=myAttr,
    row.spans=my_row_span, 
    col.spans=my_col_span,
    clean=FALSE,
    ...
  )
}

##' HTML - Source an Image
##' 
##' Convenience function for \code{cat}-ing out HTML markup for an image as 
##' \code{<img src="..." />}.
##' @param x path to an image you want to source
##' @param width width (in pixels) of the image
##' @param height height (in pixels) of the image
##' @seealso \code{\link{hImg}}
kImg <- function(x, width=480, height=480) {
  
  ## If we have a period as the first character, remove it.
  if( re_exists(x, "^\\./") ) {
    x <- gsub( "^\\./", "", x )
  }
  
  cat( paste( sep="", '<img class="center" src="', x, '" width=', width, ' height=', height, ' />') )
  
}

##' HTML - Source an SVG file
##' 
##' Convenience function for \code{cat}-ing out HTML markup for an SVG image, using 
##' \code{<embed>}.
##' @param file path to the SVG file you want to embed
##' @param width width (in pixels) of the SVG file (or, more accurately, canvas in which that file is displayed)
##' @param height height (in pixels) of the SVG file (or, more accurately, canvas in which that file is displayed)
##' @param class class passed to the \code{<embed>} tag
kSvg <- function(file=NULL, width=4, height=4, class=NULL) {
  
  stopifnot( !is.null(file) )
  ## If we have a period as the first character, remove it.
  
  if( re_exists(file, "^\\.")  ) {
    x <- gsub( "^\\.", getwd(), file )
  }
  
  tag <- paste( sep="", '<embed src="', file, '" width=', width, ' height=', height)
  if( !is.null(class) ) {
    tag <- paste( sep="", tag, " class='", class, "'" )
  }
  
  cat( "<div align='center'>\n")
  cat( paste( sep="", tag, ' type="image/svg+xml" />\n') )
  cat("</div>\n")
}

##' Print Plot to File and Return HTML
##' 
##' A convenience function that prints a plot to file, and then returns HTML
##' to embed that image in the page.
##' 
##' The \code{dim} attribute is passed on to \code{par( mfrow='dim' )}; ie, it is used if
##' you are calling a plot function that writes more than one plot.
##' 
##' The \code{png} device is used.
##' 
##' @param my_plot a plot object, or code that generates a plot
##' @param file file location to output image to
##' @param width width (in pixels) of the plot
##' @param height height (in pixels) of the plot
##' @param dpi the number of dots per inch used. Default is high to ensure plots are crisp on all displays
##' @param dim passed to \code{par( mfrow )}; used if making multiple base-R plots
##' @param scale the scale factor to use when scaling plots for web display.
##' @param device the device to use for the plot call.
##' @param ... optional arguments passed to \code{\link{png}}
##' @export
##' @examples
##' library(lattice)
##' ## generate an xyplot, write it to file, and return HTML 
##' ## code that sources the generated image
##' dat <- data.frame( x = rnorm(100), y = rnorm(100) )
##' hImg( file = "plot_output.png",
##'   xyplot( y ~ x, dat )
##' )
hImg <- function( my_plot, 
  file, 
  width=400, 
  height=300,
  dpi=300,
  dim=NULL,
  scale=100,
  device="png",
  ... ) {
  
  device <- match.fun(device)
  
  device( file, 
    width=width/scale, 
    height=height/scale, 
    res=dpi, 
    units="in", 
    ... )
  
  if( !is.null(dim) ) {
    if( length(dim) != 2 ) stop("must specifiy 2-length dimension")
    par( mfrow=dim )
  }
  
  print( my_plot )
  
  invisible( dev.off() )
  kImg( file, width=width, height=height )
}

##' Print SVG to File and Return HTML
##' 
##' A convenience function that prints a plot to file, and then returns HTML
##' to embed that image in the page. Used for SVG images.
##' 
##' @param my_plot a plot object, or code that generates a plot
##' @param file location to output file
##' @param width width (in pixels) of the plot
##' @param height height (in pixels) of the plot
##' @param scale the scale used to scale the plot from inches to pixels, for display in a web browser
##' @param dim passed to \code{par( mfrow )}; used if making multiple base-R plots
##' @param ... passed to \code{svg}
##' @export
##' @examples
##' library(lattice)
##' ## generate an xyplot, write it to file, and return HTML code that
##' ## sources the generated image
##' dat <- data.frame( x = rnorm(100), y = rnorm(100) )
##' hSvg( file = "plot_output.svg",
##'   xyplot( y ~ x, dat )
##' )
##' 
hSvg <- function( my_plot, file, width=400, height=300, dim=NULL, scale=100, ... ) {
  
  svg( file, width=width/scale, height=height/scale, ... )
  
  if( !is.null(dim) ) {
    if( length(dim) != 2 ) stop("must specifiy 2-length dimension")
    par( mfrow=dim )
  }
  
  if( any( class( my_plot ) %in% c("trellis", "ggplot") ) ) {
    print( my_plot )
  } else {
    my_plot
  }
  
  invisible( dev.off() )
  kSvg( file, width=width, height=height )
}

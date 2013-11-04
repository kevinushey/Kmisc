##' Grid Text with a Background
##' 
##' This function produces text as does \code{grid.text}, but also generates
##' a background rectangle through \code{grid.rect}. Helpful for plotting
##' e.g. overlaying correlation statistics on a plot, where you'd like the
##' element to stand out a little more.
##' 
##' @importFrom grid unit gpar is.unit stringWidth stringHeight grid.rect grid.text
##' @param label A character or \code{\link{expression}} vector. 
##' Other objects are coerced by \code{as.graphicsAnnot}.
##' @param x A numeric vector or unit object specifying x-values.
##' @param y A numeric vector or unit object specifying y-values.
##' @param just The justification of the text relative to its (x, y) location. 
##' If there are two values, the first value specifies horizontal justification 
##' and the second value specifies vertical justification. Possible string values 
##' are: \code{"left"}, \code{"right"}, \code{"centre"}, \code{"center"}, 
##' \code{"bottom"}, and \code{"top"}. 
##' For numeric values, 0 means left alignment and 1 means right alignment.
##' @param hjust A numeric vector specifying horizontal justification. 
##' If specified, overrides the \code{just} setting.
##' @param vjust A numeric vector specifying vertical justification. 
##' If specified, overrides the \code{just} setting.
##' @param check.overlap A logical value to indicate whether to check for 
##' and omit overlapping text.
##' @param default.units A string indicating the default units to use 
##' if \code{x} or \code{y} are only given as numeric vectors.
##' @param name A character identifier.
##' @param gp An object of class \code{gpar}, typically the output from a call to the function
##' \code{gpar}. This is basically a list of graphical parameter settings.
##' @param draw A logical value indiciating whether graphics output should be produced.
##' @param vp A Grid viewport object (or \code{NULL}).
##' @param widthAdj A width adjustment parameter, to help control how much horizontal padding
##' there should be between the text and the background rectangle.
##' @param heightAdj A height adjustment parameter, to help control how much
##' vertical padding there should be between the text and the background rectangle.
##' @export
##' @seealso \code{\link{grid.text}} and \code{\link{grid.rect}}
##' @examples
##' x <- rnorm(10)
##' y <- rnorm(10)
##' if (require(lattice)) xyplot( y ~ x,
##'         panel = function(x, y, ...) {
##'           panel.xyplot(x, y, ...)
##'           grid.text2("some text\nwith a nice\nbackground")
##'           grid.text2( expression( sum(x[i], i==1, n)^2 ), x=0.8, y=0.8 )
##'           grid.text2( paste("sum of rnorm(10): ", sum(rnorm(10)) ), x=0.2, y=0.2, just="left" )
##'           grid.text2( "horizontal justifications work too", x=0.95, y=0.35, just="right" )
##'         })
##' ## will work for multi-panel plots as well
grid.text2 <- function(label, 
                       x=unit(0.5, "npc"), 
                       y=unit(0.5, "npc"),
                       just="centre",
                       hjust=NULL,
                       vjust=NULL,
                       check.overlap=FALSE,
                       default.units="npc",
                       name=NULL,
                       gp=gpar(col="black", 
                               fill="grey92", 
                               lineend="butt", 
                               linejoin="round"
                               ),
                       draw=TRUE,
                       vp=NULL,
                       widthAdj=unit(0.05, "npc"),
                       heightAdj=unit(0.05, "npc")
                       ) {
  
  if( !is.unit(x) ) {
    x <- unit(x, "npc")
  }
  
  if( !is.unit(y) ) {
    y <- unit(y, "npc")
  }
  
  x_rect <- x
  y_rect <- y
  
  strWidth <- stringWidth(label)
  strHeight <- stringHeight(label)
  
  if( isTRUE( just[1] == "left" ) | isTRUE( hjust == 0 ) ) {
    x_rect <- x - widthAdj * 0.5 
  }
  
  if( isTRUE( just[1] == "right" ) | isTRUE( hjust == 1 ) ) {
    x_rect <- x + widthAdj * 0.5
  }
  
  grid.rect(x=x_rect, 
            y=y_rect, 
            width=strWidth + widthAdj, 
            height=strHeight + heightAdj,
            just=just,
            hjust=hjust,
            vjust=vjust,
            default.units=default.units,
            name=name,
            gp=gp,
            draw=draw
  )
  
  grid.text(label=label,
            x=x, 
            y=y, 
            just=just,
            hjust=hjust,
            vjust=vjust,
            rot=0,
            check.overlap=check.overlap,
            default.units=default.units,
            name=name,
            gp=gp,
            draw=draw,
            vp=vp
  )
  
}

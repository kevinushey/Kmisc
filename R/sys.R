##' Invoke a System Command
##' 
##' This function wraps to \code{system}, but interprets all un-named
##' arguments as things to be \code{paste}-ed. See \code{\link{system}}
##' for further details.
##' @param ... System command to be invoked; this gets passed into
##' \code{paste(..., sep='', collapse='')}.
##' @param intern A logical (not \code{NA}) which indicates whether to
##' capture the output of the command as an \R character vector.
##' @param ignore.stdout Ignore \code{stdout}?
##' @param ignore.stderr Ignore \code{stderr}?
##' @param wait Should the \R interpreter wait for the program to finish
##' execution?
##' @param input If a character vector is supplied, this is copied one string
##' per line to a temporary file, and the standard input of \code{...} is
##' redirected to the file.
##' @param show.output.on.console Windows only -- show output on console?
##' @param minimized Windows only -- run the shell minimized?
##' @param invisible Windows only -- run invisibly?
sys <- function(..., 
                intern=FALSE, 
                ignore.stdout=FALSE,
                ignore.stderr=FALSE,
                wait=TRUE,
                input=NULL,
                show.output.on.console=TRUE,
                minimized=FALSE,
                invisible=TRUE
                ) {
  
  if( .Platform$OS.type == "windows" ) {
    return( system( paste(..., sep='', collapse=''),
                  intern=intern,
                  ignore.stdout=ignore.stdout,
                  ignore.stderr=ignore.stderr,
                  wait=wait,
                  input=input,
                  show.output.on.console=show.output.on.console,
                  minimized=minimized,
                  invisible=invisible
  ) )
  } else {
    return( system( paste(..., sep='', collapse=''),
                    intern=intern,
                    ignore.stdout=ignore.stdout,
                    ignore.stderr=ignore.stderr,
                    wait=wait,
                    input=input
    ) )
  }
  
}

ChangeLog <- function(msg="") {
  
  if (!exists(".Start.time")) {
    stop("No variable '.Start.time' available; unable to infer what ",
      "files have been modified in the current session!\n",
      "Try adding `.Start.time <- as.numeric( Sys.time() )` to your .Rprofile.")
  }
  
  if (!file.exists("ChangeLog")) {
    message("Creating a new ChangeLog file...")
    file.create("ChangeLog")
  }
  
  ## Find out what files have been modified in the current session
  files <- list.files(full.names=TRUE, include.dirs=FALSE, recursive=TRUE)
  
  times <- unlist( lapply(files, function(x) {
    as.numeric(system(paste("stat -f%c", x), intern=TRUE))
  }))
  
  modified <- files[ times > .Start.time ]
  
  ## The ChangeLog is not a candidate for listing in modifies
  modified <- modified[ modified != "./ChangeLog" ]
  
  ## Strip off the initial './'
  modified <- substring(modified, 3, nchar(modified))
  
  if (length(modified)) {
    
    ## Generate the header
    header <- paste0( Sys.Date(), "  ", Sys.getenv("USERNAME"), " <", Sys.getenv("EMAIL"), ">" )
    
    ## Stubs for each modified file
    body <- paste0("        * ", modified, ": ", msg)
    
    changes <- c(header, "", body)
    
    ## Read in the ChangeLog, put the new changes on top, and write it back out
    ChangeLog <- readLines("ChangeLog")
    New <- c(changes, ChangeLog)
    
    ## Write out the new ChangeLog, and open it in an editor
    cat(New, file="ChangeLog", sep="\n")
    return( file.edit("ChangeLog") )
  } else {
    message("No files have been modified in the current session.")
    return (invisible(NULL))
  }
  
}

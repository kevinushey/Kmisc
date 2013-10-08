shiny.package.skeleton <- function(name, libs=c("jQuery", "jQueryUI", "dialogExtend")) {
  
  if (file.exists(name)) {
    stop("Directory '", name, "' already exists!")
  }
  message("Creating directory ", name, "...")
  dir.create(name)
  setwd(name)
  dir.create("www")
  dir.create("www/js")
  dir.create("www/css")
  dir.create("www/images")
  
  if ("jQuery" %in% libs) {
    message("Downloading latest version of jQuery...")
    download.file("http://code.jquery.com/jquery.min.js", "www/js/jquery.js")
  }
  
  if ("jQueryUI" %in% libs) {
    message("Downloading latest version of jQueryUI...")
    tempfile <- tempfile()
    download.file("http://jqueryui.com/resources/download/jquery-ui-1.10.3.zip", tempfile)
    tempdir <- tempdir()
    unzip(tempfile, exdir=tempdir)
    folder <- grep("jquery-ui", list.files(tempdir, full.names=TRUE), value=TRUE)
    if (length(folder) != 1) {
      stop("Could not locate the jQueryUI folder after unzipping!")
    }
    files <- list.files(folder, recursive=TRUE, full.names=TRUE)
    src_file <- grep("ui/minified/jquery-ui.min.js", files, value=TRUE) 
    if (length(src_file) != 1) {
      stop("Could not locate jQuery-ui.min.js!")
    }
    file.copy(src_file, "www/js/jquery-ui.js")
    
    ## copy the themes and icons as well
    css_file <- grep("jquery-ui.css", files, value=TRUE)
    if (length(css_file) != 1) {
      stop("Could not locate jQuery-ui.css!")
    }
    file.copy(css_file, "www/css/jquery-ui.css")
    
    #imgs
    imgs <- grep("/themes/base/images/", files, value=TRUE)
    for (img in imgs) {
      file.copy(img, file.path("www", "images", basename(img)))
    }
    
    ## delete the downloaded files
    file.remove(tempfile)
    system( paste("rm -rf", folder) )
  }
  
  if ("dialogExtend" %in% libs) {
    message("Downloading latest version of dialogExtend...")
    download.file("https://raw.github.com/ROMB/jquery-dialogextend/master/build/jquery.dialogextend.min.js",
      "www/js/jquery.dialogextend.min.js",
      method="wget"
    )
  }
  
  message("Generating 'ui.R'...")
  file.copy( system.file("resources/shiny/ui.R", package="Kmisc"), "ui.R" )
  message("Generating 'server.R'...")
  file.copy( system.file("resources/shiny/server.R", package="Kmisc"), "server.R")
  message("Done!")
}

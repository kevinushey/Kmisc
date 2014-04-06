## This function:
## 1. Concatenates all .c files into <pkg>_c.c (excluding an init.c file),
## 2. Concatenates all .cpp files into <pkg>_cpp.cpp,
## This greatly speeds compilation time.
cleanup <- function() {
  
  ## Bail if this is an R CMD INSTALL
  if (Sys.getenv("R_INSTALL_PKG") != "") {
    message("It looks like this isn't being called from R CMD build; bailing out")
    return(invisible(NULL))
  }
  
  stopifnot(file.exists("DESCRIPTION"))
  
  ## get the package name from the DESCRIPTION file
  DESCRIPTION <- as.list(read.dcf("DESCRIPTION")[1, ])
  pkg_name <- DESCRIPTION$Package
  pkg_version <- DESCRIPTION$Version
  
  ## Since we are in the R CMD build step, we can just build in the current dir
  buildDir <- getwd()
  
  ## in the build directory, 'cat' all the src files together
  ## have a separate file for .c, .cpp files
  src_files <- list.files( full.names=TRUE,
    file.path( buildDir, "src" )
  )
  
  ## but don't concatenate init.c; copy it separately
  src_files <- grep("init\\.c$", src_files, value=TRUE, invert=TRUE)
  
  ## regex: the regex to match for picking up files
  ## ext: the file extension to use on the outputted file
  concatenate_src <- function(regex, ext) {
    files <- grep(regex, src_files, value=TRUE)
    final <- paste( sep='', buildDir, "/src/", pkg_name, "_", gsub("\\.", "", ext), ext )
    file.create(final)
    files <- files[ files != final ]
    for (file in files) {
      header <- paste("// begin file", basename(file), "\n\n")
      footer <- paste("// end file", basename(file), "\n\n")
      cat(header, file = final, append = TRUE)
      system( paste("cat", shQuote(file), ">>", shQuote(final)) )
      cat(footer, file = final, append = TRUE)
    }
    file.remove(files)
  }
  
  cat("Concatenating source files...\n")
  concatenate_src("\\.c$", ".c")
  concatenate_src("\\.cpp$", ".cpp")
  
}

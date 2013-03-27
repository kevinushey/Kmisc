#' Prepare Package
#' 
#' This function prepares the package such that all the C / C++ source files
#' are concatenated into one file (for each source). This increases
#' compilation time, and produces a tarball that can be used for submission
#' to CRAN.
#' 
#' @param build Build the package with \code{R CMD build}?
#' @param check Check the package with \code{R CMD check}?
#' @param install Install the package with \code{R CMD INSTALL}? Only be
#' done if \code{build} is \code{TRUE} as well.
prepare_package <- function(build=TRUE, check=TRUE, install=FALSE) {
  
  cat("Copying files to build directory...\n")
  ## check for necessary package files
  files <- list.files()
  stopifnot( "DESCRIPTION" %in% files & "src" %in% files )
  
  ## get the package name from the DESCRIPTION file
  DESCRIPTION <- readLines("DESCRIPTION")
  pkg_name <- gsub("Package: ", "", grep("^Package:", DESCRIPTION, value=TRUE))
  pkg_version <- gsub("Version: ", "", grep("^Version:", DESCRIPTION, value=TRUE))
  
  ## copy the files to a 'build' directory
  buildDir <- paste(sep='', "../", pkg_name, "_", pkg_version, "_tmp_build")
  if( file.exists(buildDir) ) {
    system( paste("rm -rf", shQuote(buildDir)) )
  }
  dir.create(buildDir, showWarnings=FALSE)
  system( paste("cp -r .", buildDir) )
  
  ## in the build directory, 'cat' all the src files together
  ## have a separate file for .c, .cpp files
  src_files <- list.files( full.names=TRUE, paste( sep='',
                                  buildDir, "/src"
  ) )
  
  ## regex: the regex to match for picking up files
  ## ext: the file extension to use on the outputted file
  concatenate_src <- function(regex, ext) {
    files <- grep( regex, src_files, value=TRUE )
    final <- paste( sep='', buildDir, "/src/", pkg_name, "_", gsub("\\.", "", ext), ext )
    system( paste("touch", final) )
    files <- files[ files != final ]
    files <- files[ !(files %in% grep("RcppExports", files, value=TRUE)) ]
    for( file in files ) {
      system( paste("cat", shQuote(file), ">>", shQuote(final)) )
      system( paste("echo '' >>", shQuote(final) ) ) ## adds a newline just in case
      system( paste("rm", file) )
    }
  }
  
  cat("Concatenating source files...\n")
  concatenate_src("c$", ".c")
  concatenate_src("cpp$", ".cpp")
  
  ## remove all the .o, .so, .dll files
  for( file in grep("o$|so$|dll$", list.files( full.names=TRUE, file.path( buildDir, "src" ) ), value=TRUE ) ) {
    file.remove(file)
  }
  
  cat("Building...\n\n")
  ## build the package
  if(build) {
    system( paste("R CMD build", paste(sep='', "../", pkg_name, "_", pkg_version, "_tmp_build") ) )
    system( paste("mv", paste(sep='', pkg_name, "_", pkg_version, ".tar.gz"), "../") )
    cat("Done!\n\n")
  }
  
  ## check the package
  if(check) {
    cat("Running R CMD check...\n\n")
    system( paste("R CMD check", paste(sep='', "../", pkg_name, "_", pkg_version, ".tar.gz")))
    system( paste("rm -rf", paste0(pkg_name, ".Rcheck")) )
  }
  
  ## install the package
  if(build && install) {
    cat("Installing package...\n\n")
    system( paste("R CMD INSTALL --preclean --no-multiarch", paste(sep='', "../", pkg_name, "_", pkg_version, ".tar.gz")))
    cat("Done!\n")
  }
  
  ## remove the build dir
  system( paste("rm -rf", buildDir) )
  
}
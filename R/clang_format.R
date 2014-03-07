clang_format <- function(
  files=NULL,
  style=c("Chromium", "LLVM", "Google", "Mozilla", "WebKit"),
  verbose=FALSE) {
  
  style <- match.arg(style)
  
  if (is.null(files)) {
    files <- list.files("src", pattern="cpp$|c$", full.names=TRUE)
  }
  
  files <- unlist( lapply(files, function(file) {
    normalizePath(file, mustWork=TRUE)
  }) )
  
  styleArg <- paste0("-style=", shQuote(style))
  iArg <- "-i"
  cmd <- paste("clang-format", styleArg, iArg, paste(files, collapse=" "))
  if (verbose) {
    message(cmd)
  }
  system(cmd)
}

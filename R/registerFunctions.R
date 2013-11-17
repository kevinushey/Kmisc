registerFunctions <- function(pkg_name, prefix="C") {
  
  if (missing(pkg_name))
    pkg_name <- basename( getwd() )
  
  ## read in the C files
  ## but not init.c; it's special
  files <-  list.files("src", pattern="[cC]$", full.names=TRUE)
  files <- files[ files != "src/init.c" ]
  
  c_files <- lapply(files, readLines)
  
  get_c_prototypes <- function(x) {
    export_lines <- grep("// \\[\\[export\\]\\]", x)
    sapply(export_lines, function(line) {
      return(gsub("(.*)// \\[\\[export\\]\\]\n(.*?) ?\\{(.*)", "\\2;", 
        paste(x[line:length(x)], collapse="\n")))
    })
  }
  
  c_prototypes <- sapply(c_files, get_c_prototypes)
  c_prototypes <- c_prototypes[ sapply(c_prototypes, function(x) {
    !identical(x, list())
  }) ]
  
  rcpp_exports <- readLines("src/RcppExports.cpp")
  fn_lines <- grep("^RcppExport", rcpp_exports, value=TRUE)
  cpp_prototypes <- sapply(fn_lines, USE.NAMES=FALSE, function(x) {
    gsub("RcppExport (.*) \\{", "\\1;", x)
  })
  
  all_prototypes <- unlist(c(c_prototypes, cpp_prototypes))
  
  all_names <- sapply( all_prototypes, function(x) {
    gsub("SEXP (.*)\\(.*", "\\1", x)
  })
  
  all_nargs <- sapply(all_prototypes, function(x) {
    defn <- gsub("SEXP (.*)\\((.*)\\).*", "\\2", x)
    m <- gregexpr("SEXP +", defn)
    if (identical( as.integer(m[[1]]), -1L )) {
      return(0)
    } else {
      return (length(m[[1]]))
    }
  })
  
  Cnames <- paste0(prefix, all_names)
  
  cmd_lines <- paste0("{\"", Cnames, "\", (DL_FUNC) &", all_names, ", ", all_nargs, "},")
  
  R_CallMethodsDef <- c(
    "R_CallMethodDef callMethods[]  = {",
    paste0("  ", cmd_lines),
    "  {NULL, NULL, 0}",
    "};"
  )
  
  R_RegisterRoutines <- c(
    paste0("void R_init_", pkg_name, "(DllInfo *info) {"),
    "  R_registerRoutines(info, NULL, callMethods, NULL, NULL);",
    "  R_useDynamicSymbols(info, FALSE);",
    "}"
  )
  
  ## write it out to an init file
  init.c <- c(
    "#include <R.h>",
    "#include <Rinternals.h>",
    "",
    "#include <R_ext/Rdynload.h>",
    "",
    all_prototypes,
    "",
    R_CallMethodsDef,
    "",
    R_RegisterRoutines,
    ""
  )
  
  cat(init.c, file="src/init.c", sep="\n")
  return(invisible(NULL))
  
}

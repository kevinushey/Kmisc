Rcpp.gallery.skeleton <- function(
  title,
  author=Sys.getenv("USER"),
  license="GPL (>= 2)",
  tags=NULL,
  summary="",
  file,
  style="markdown"
) {
  
  ext <- switch(style, markdown=".Rmd", Rmarkdown=".Rmd", cpp=".cpp", 
    stop("Unrecognized style: recognized styles are '[R]markdown' and 'cpp',")
  )
  
  ## Use a temporary file location by default
  if (missing(file)) {
    file <- tempfile(fileext=ext)
  }
  
  ## Append the appropriate extension if necessary
  if (!grepl( paste0(ext, "$"), file)) {
    file <- paste0(file, ext)
  }
  
  switch(style,
  markdown=.Rcpp.gallery.skeleton.markdown(title=title,
    author=author, license=license, tags=tags, summary=summary, file=file),
  cpp=.Rcpp.gallery.skeleton.cpp(title=title,
    author=author, license=license, tags=tags, summary=summary, file=file)
  )
  
}

.Rcpp.gallery.skeleton.markdown <- function(title, author, license, tags, summary, file) {
  
  header <- c(
    "---",
    paste("title:", title),
    paste("author:", author),
    paste("license:", license),
    paste("tags:", paste(tags, collapse=", ")),
    paste("summary:", summary),
    "---",
    "",
    "```{r, engine='Rcpp'}",
    "#include <Rcpp.h>",
    "using namespace Rcpp;",
    "",
    "// [[Rcpp::export]]",
    "int addOne(int x) {",
    "    return x + 1;",
    "}",
    "```"
  )
  
  cat(header, sep="\n", file=file)
  file.edit(file)
  
}

.Rcpp.gallery.skeleton.cpp <- function(title, author, license, tags, summary, file) {
  
  header <- c(
    "/***",
    paste(" * @title:", title),
    paste(" * @author:", author),
    paste(" * @license:", license),
    paste(" * @tags:", paste(tags, collapse=", ")),
    paste(" * @summary:", summary),
    " */",
    "",
    "#include <Rcpp.h>",
    "using namespace Rcpp;",
    "",
    "/**",
    " * ## Introduction",
    " * Text within block comments is interpretted as Markdown on compilation.",
    " */",
    "",
    "// [[Rcpp::export]]",
    "int addOne(int x) {",
    "    return x + 1;",
    "}"
  )
  
  cat(header, sep="\n", file=file)
  file.edit(file)
  
}

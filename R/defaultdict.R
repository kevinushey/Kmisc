##' Python-style Default Dictionaries in R
defaultdict <- function(..., `_size` = 29L) {
  dots <- list(...)
  dict <- new.env(parent = emptyenv(), size = max(`_size`, length(dots)))
  names <- names(dots)
  for (i in seq_along(names)) {
    dict[[ names[i] ]] <- dots[[i]]
  }
  class(dict) <- c("defaultdict", "dict")
  dict
}

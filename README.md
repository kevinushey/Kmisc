# Kmisc

Kmisc is a package chock full of miscellaneous functions that intend to make
the R programming process easier. The functions range from:

  * A faster implementation of `reshape2::melt` for `data.frame`s and `matrix`'s,
  * Utility functions for generating and styling R Markdown documents,
  * A simple wrapper to `awk`,
  * Functions for reading in subsets of very large files `extract_rows_from_file`,
  * Faster implementations of some common R functions; e.g. `counts` is a faster
  version of `table` for single vectors; `tapply_` is a faster `tapply` for the
  common case of splitting an atomic vector by another atomic vector,
  `factor_` is a faster `factor`...
 * And more! Browse the index to get an idea of everything that's there.

Install me in R with `devtools::install_github` with the following call:

    devtools::install_github("Kmisc", "kevinushey", subdir="Kmisc")
    
(the `subdir` argument is necessary as I include source tarballs in the base directory, and the actual source itself in this sub-directory)

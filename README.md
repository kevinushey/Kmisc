[![Build Status](https://travis-ci.org/kevinushey/Kmisc.png)](https://travis-ci.org/kevinushey/Kmisc)

# Kmisc

Kmisc is a package chock full of miscellaneous functions that intend to make
the R programming process easier. The functions range from:

  * A faster implementation of `reshape2::melt` for `data.frame`s and `matrix`'s,
  * Utility functions for generating and styling R Markdown documents,
  * A simple wrapper to `awk`,
  * Functions for reading in subsets of very large files:
  * * `extract_rows_from_file` pulls specific rows from a (potentially large)
  tabular data file, where we only take rows for which a particular column
  entry matches a list of desired elements,
  * * `split_file` splits a file on a particular column into separate, smaller
  files, so that these smaller files are more amenable to parallel processing,
  * Faster implementations of some common R functions; e.g. `counts` is a faster
  version of `table` for single vectors; `tapply_` is a faster `tapply` for the
  common case of splitting an atomic vector by another atomic vector,
  `factor_` is a faster `factor`...
  * And more! Browse the index to get an idea of everything that's there.

Install me in R with `devtools::install_github` with the following call:

    devtools::install_github("Kmisc", "kevinushey")

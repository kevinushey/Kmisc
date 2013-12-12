<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Introduction to Kmisc}
\usepackage[utf8]{inputenc}
-->

Introduction to Kmisc
=====

Kmisc introduces a grab-bag of utility functions that should be useful to various
kinds of `useR`s. Some of the most useful functions in the package are demoed 
in this vignette.


```r
set.seed(123)
library(data.table)
library(Kmisc)
library(lattice)
library(grid)
library(Rcpp)
library(knitr)
library(microbenchmark)
dat <- data.frame( x=letters[1:4], y=1:4, z=LETTERS[1:4] )
opts_chunk$set(
  results="markup"
)
```


`without`: This function is used to remove columns from a `list` / `data.frame`.


```r
## let's remove columns 'x' and 'z' from dat.
tryCatch( dat[ -c('x', 'z') ], error=function(e) print(e$message) )
```

```
## [1] "invalid argument to unary operator"
```

```r
## oh :(
dat[ !(names(dat) %in% c('x', 'z')) ]
```

```
##   y
## 1 1
## 2 2
## 3 3
## 4 4
```

```r
## I always find that a bit awkward. Let's use Kmisc's without instead.
without(dat, x, z)
```

```
##   y
## 1 1
## 2 2
## 3 3
## 4 4
```


`extract`: Extract vectors from a data.frame or list. Although there is already
a good subsetting syntax for lists and vectors, I wanted a complementary
function for `without`.


```r
extract(dat, x, y)
```

```
##   x y
## 1 a 1
## 2 b 2
## 3 c 3
## 4 d 4
```


`re_without, re_extract`: Extract variables whose names don't match / do match
a regular expression pattern.


```r
re_extract(dat, "[xy]")
```

```
##   x y
## 1 a 1
## 2 b 2
## 3 c 3
## 4 d 4
```

```r
re_without(dat, "[xy]")
```

```
##   z
## 1 A
## 2 B
## 3 C
## 4 D
```


`swap`: Replace elements in a vector.


```r
tDat <- dat ## make a temporary copy of dat

## Replace some elements in tDat$y
tDat$y <- swap( tDat$y, from=c(2, 4), to=c(20, 40) )
cbind( dat$y, tDat$y )
```

```
##      [,1] [,2]
## [1,]    1    1
## [2,]    2   20
## [3,]    3    3
## [4,]    4   40
```


`factor_to_char`, `char_to_factor`: A set of functions that recurse through
a list / data.frame and set all elements that are characters to factors,
and vice versa.


```r
bDat <- data.frame( x=rnorm(10), y=sample(letters,10), z=sample(letters,10) )
str( bDat )
```

```
## 'data.frame':	10 obs. of  3 variables:
##  $ x: num  -0.5605 -0.2302 1.5587 0.0705 0.1293 ...
##  $ y: Factor w/ 10 levels "c","f","k","l",..: 10 7 6 9 5 8 3 4 2 1
##  $ z: Factor w/ 10 levels "a","d","e","f",..: 10 9 7 8 1 5 6 3 4 2
```

```r
str( factor_to_char(bDat) )
```

```
## 'data.frame':	10 obs. of  3 variables:
##  $ x: num  -0.5605 -0.2302 1.5587 0.0705 0.1293 ...
##  $ y: chr  "x" "r" "p" "w" ...
##  $ z: chr  "z" "w" "q" "s" ...
```


`dapply`: The `data.frame` version of the `l/sapply` series of functions.

Why have this function when `sapply` still does much the same? I always get
frustrated with the fact that either an `array` or a `list` is returned
by sapply, but never a `data.frame`.


```r
dat <- data.frame( x = rnorm(100), y = rnorm(100), z = rnorm(100) )
dapply( dat, summary )
```

```
##               x      y       z
## Min.    -2.3100 -2.050 -2.0100
## 1st Qu. -0.6260 -0.728 -0.5740
## Median  -0.0587 -0.206  0.0535
## Mean    -0.0098 -0.024  0.1010
## 3rd Qu.  0.5620  0.572  0.7790
## Max.     2.1900  3.240  2.2900
```


`kMerge`: Left joins, aka. `merge( all.x=TRUE, ... )` without any mangling 
of the order.


```r
dat1 <- data.frame( id=5:1, x=c("a","a","b","b","b"), y=rnorm(5) )
dat2 <- data.frame( id=c(1, 2, 4), z=rnorm(3) )

## default merge changes id order
merge( dat1, dat2, by="id", all.x=TRUE )
```

```
##   id x       y        z
## 1  1 b -0.5229  0.68375
## 2  2 b  0.6608 -0.06082
## 3  3 b -1.3388       NA
## 4  4 a  1.2181  0.63296
## 5  5 a  0.2374       NA
```

```r
## even the sort parameter can't save you
merge( dat1, dat2, by="id", all.x=TRUE, sort=TRUE )
```

```
##   id x       y        z
## 1  1 b -0.5229  0.68375
## 2  2 b  0.6608 -0.06082
## 3  3 b -1.3388       NA
## 4  4 a  1.2181  0.63296
## 5  5 a  0.2374       NA
```

```r
# kMerge keeps it as is
kMerge( dat1, dat2, by="id" )
```

```
##   id x       y        z
## 1  5 a  0.2374       NA
## 2  4 a  1.2181  0.63296
## 3  3 b -1.3388       NA
## 4  2 b  0.6608 -0.06082
## 5  1 b -0.5229  0.68375
```


`in_interval`: A fast C implementation for determing which elements of a 
vector `x` lie within an interval `[lo, hi)`.


```r
x <- runif(10)*10; lo <- 5; hi <- 10
print( data.frame( x=x, between_5_and_10=in_interval(x, lo, hi) ) )
```

```
##         x between_5_and_10
## 1  9.0915             TRUE
## 2  0.5638            FALSE
## 3  5.0291             TRUE
## 4  3.5054            FALSE
## 5  8.4556             TRUE
## 6  8.0644             TRUE
## 7  1.1733            FALSE
## 8  7.1269             TRUE
## 9  2.3527            FALSE
## 10 0.7496            FALSE
```


`stack_list`: Use this to stack data.frames in a list. This can be useful if
e.g. you've run some kind of bootstrap procedure and have all your results
stored in as a list of data.frames -- even `do.call( rbind, dfs )` can be slow.
The difference is even more prominent when used on very large lists.

This is partially deprecated by `data.table::rbindlist` now, which has a much 
faster C implementation.


```r
dfs <- replicate(1E3, 
  data.frame(x=rnorm(10), y=sample(letters,10), z=sample(LETTERS,10)),
  simplify=FALSE
  )
str( stack_list(dfs) )
```

```
## 'data.frame':	10000 obs. of  4 variables:
##  $ x         : num  1.519 0.377 -2.052 -1.364 -0.201 ...
##  $ y         : chr  "n" "e" "l" "f" ...
##  $ z         : chr  "E" "C" "K" "I" ...
##  $ list_index: int  1 1 1 1 1 1 1 1 1 1 ...
```

```r
system.time( stack_list(dfs) )
```

```
##    user  system elapsed 
##   0.004   0.000   0.004
```

```r
system.time( do.call(rbind, dfs) )
```

```
##    user  system elapsed 
##   0.258   0.039   0.298
```

```r
system.time( data.table::rbindlist(dfs) )
```

```
##    user  system elapsed 
##   0.002   0.000   0.002
```


Fast String Operations
-----

R is missing some nice builtin 'string' functions. I've
introduced a few functions for common string operations.

`str_rev`: Reverses a character vector; ie, a vector of strings.
`str_rev2` is there if you need to reverse a potentially unicode string.


```r
str_rev( c("ABC", "DEF", NA, paste(LETTERS, collapse="") ) )
```

```
## [1] "CBA"                        "FED"                       
## [3] NA                           "ZYXWVUTSRQPONMLKJIHGFEDCBA"
```

```r
str_rev2( c("はひふへほ", "abcdef") )
```

```
## [1] "ほへふひは" "fedcba"
```


`str_slice`: Slices a vector of strings at consecutive indices `n`.
`str_slice2` exists for potentially unicode strings.

```r
str_slice( c("ABCDEF", "GHIJKL", "MNOP", "QR"), 2 )
```

```
## [[1]]
## [1] "AB" "CD" "EF"
## 
## [[2]]
## [1] "GH" "IJ" "KL"
## 
## [[3]]
## [1] "MN" "OP"
## 
## [[4]]
## [1] "QR"
```

```r
str_slice2( "ハッピー", 2 )
```

```
## [[1]]
## [1] "ハッ" "ピー"
```


`str_sort`: sort a string.

```r
str_sort("asnoighewgypfuiweb")
```

```
## [1] "abeefgghiinopsuwwy"
```


`str_collapse`: Collapse a string using `Rcpp` sugar; operates like
R's `paste(..., collapse="")`, but works much faster.

```r
str_collapse( c("ABC", "DEF", "GHI") )
```

```
## [1] "ABCDEFGHI"
```



File I/O
-----

Sometimes, you get really large data files that just aren't going to fit into
RAM. You really wish you could split them up in a structured way, transform
them in some way, and then put them back together. One might consider a more
'enterprise' edition of the split-apply-combine framework (Hadoop and friends),
but one dirty alternative is to use C++ to munge through a text file and pull out
things that we actually want.

`split_file`: This function splits a delimited file into multiple files, according to
unique entries in a chosen column.

`extract_rows_from_file`: From a delimited text file, extract only the rows for
which the entries in a particular column match some set of items that you
wish to keep.

C++ Function Generators
-----

Use these functions to generate C++ / Rcpp-backed functions for common R-style
operations.

`Rcpp_tapply_generator`: Generate fast `tapply` style functions from C++
code through Rcpp. See the example.


```r
dat <- data.frame( y=rnorm(100), x=sample(letters[1:5], 100, TRUE) )
tMean <- Rcpp_tapply_generator("return mean(x);")
```

```
## C++ source code will be written to /var/folders/m7/_xnnz_b53kjgggkb1drc1f8c0000gn/T//RtmpQmooT2/file4094e9199d1.cpp .
## Compiling...
## Done!
```

```r
with( dat, tMean(y, x) )
```

```
##      a      b      c      d      e 
## 0.3928 0.1372 0.1822 0.2425 0.4269
```

```r
with( dat, tapply(y, x, mean) )
```

```
##      a      b      c      d      e 
## 0.3928 0.1372 0.1822 0.2425 0.4269
```

```r
microbenchmark(
  Kmisc=with( dat, tMean(y, x) ),
  R=with( dat, tapply(y, x, mean) ),
  times=5
)
```

```
## Unit: microseconds
##   expr    min     lq median     uq    max neval
##  Kmisc  46.23  51.11  52.23  54.18  81.17     5
##      R 187.76 189.22 193.71 198.47 518.60     5
```


`Rcpp_apply_generator`: An apply function generator tailored to 2D matrices.
However, your function definition must return a scalar value.


```r
aMean <- Rcpp_apply_generator("return mean(x);")
```

```
## C++ source code will be written to /var/folders/m7/_xnnz_b53kjgggkb1drc1f8c0000gn/T//RtmpQmooT2/file40945a8bbd1d.cpp .
## Compiling...
## Done!
```

```r
mat <- matrix( rnorm(100), nrow=10 )
aMean(mat, 2)
```

```
##  [1] -0.76100 -0.85464  0.19350  0.11695 -0.40288 -0.01592  0.20021
##  [8] -0.29928 -0.78401 -0.43277
```

```r
apply(mat, 2, mean)
```

```
##  [1] -0.76100 -0.85464  0.19350  0.11695 -0.40288 -0.01592  0.20021
##  [8] -0.29928 -0.78401 -0.43277
```

```r
microbenchmark(
  Kmisc=aMean(mat, 2),
  R=apply(mat, 2, mean)
)
```

```
## Unit: microseconds
##   expr    min     lq  median      uq    max neval
##  Kmisc  5.298  6.146   6.862   8.432  20.58   100
##      R 94.858 97.196 100.856 103.036 161.92   100
```


Faster Versions of Commonly Used R Functions
--------------------------------------------

`tapply_`: This function operates like `tapply` but works faster through a 
faster factor generating function, as well as an optimized split. Note that
it is however restricted to the (common) case of your value and grouping
variables being column vectors.


```r
library(microbenchmark)
y <- rnorm(1000); x <- sample(letters[1:5], 1000, TRUE)
tapply(y, x, mean)
```

```
##         a         b         c         d         e 
##  0.092720  0.005166  0.039360 -0.033520 -0.037047
```

```r
tapply_(y, x, mean)
```

```
##         a         b         c         d         e 
##  0.092720  0.005166  0.039360 -0.033520 -0.037047
```

```r
microbenchmark( times=10,
  tapply(y, x, mean),
  tapply_(y, x, mean),
  tMean(y, x)
)
```

```
## Unit: microseconds
##                 expr    min     lq median    uq   max neval
##   tapply(y, x, mean) 299.60 308.27 325.26 361.0 619.3    10
##  tapply_(y, x, mean)  89.31  90.97  92.43  99.7 181.5    10
##          tMean(y, x)  70.05  73.41  76.24 116.6 298.0    10
```


`melt_`: This function operates like `reshape2:::melt`, but works almost
entirely through C and hence is much faster. 


```r
dat <- data.frame(
  id=LETTERS[1:5],
  x1=rnorm(5),
  x2=rnorm(5),
  x3=rnorm(5)
)
print(dat)
```

```
##   id       x1      x2      x3
## 1  A -0.07795 -0.7611  1.2437
## 2  B -0.53704 -2.5297 -0.4835
## 3  C -0.93115  1.1976 -1.3957
## 4  D -0.33415  0.5021 -0.1184
## 5  E -0.21133 -0.3940  0.4228
```

```r
melt_(dat, id.vars="id")
```

```
##    id variable    value
## 1   A       x1 -0.07795
## 2   B       x1 -0.53704
## 3   C       x1 -0.93115
## 4   D       x1 -0.33415
## 5   E       x1 -0.21133
## 6   A       x2 -0.76112
## 7   B       x2 -2.52968
## 8   C       x2  1.19765
## 9   D       x2  0.50212
## 10  E       x2 -0.39404
## 11  A       x3  1.24369
## 12  B       x3 -0.48355
## 13  C       x3 -1.39575
## 14  D       x3 -0.11838
## 15  E       x3  0.42275
```


`factor_`: A faster, simpler implementation of `factor` through Rcpp. This might
be useful in some rare cases where speed is essential.


```r
lets <- sample(letters, 1E6, TRUE)
stopifnot( identical(
  factor_(lets),
  factor(lets)
) )
microbenchmark( times=5,
  factor_(lets),
  factor(lets)
)
```

```
## Unit: milliseconds
##           expr   min    lq median    uq   max neval
##  factor_(lets) 10.68 10.76  10.95 12.47 14.50     5
##   factor(lets) 38.77 40.60  41.15 42.85 43.01     5
```


`html`: Custom HTML in an R Markdown document.


```r
html(
  table( class="table table-bordered table-striped table-condensed table-hover", ## bootstrap classes
    tr(
      td("Apples"),
      td("Bananas")
    ),
    tr(
      td("20"),
      td("30")
    )
  )
)
```

<table class='table table-bordered table-striped table-condensed table-hover'>
<tr>
<td>
Apples
</td><td>
Bananas
</td>
</tr><tr>
<td>
20
</td><td>
30
</td>
</tr>
</table> 


`anatomy`, `anat`: Like `str`, but much faster. It won't choke on very large `data.frame`s.


```r
df <- data.table(x=1, y=2)
str(df)
```

```
## Classes 'data.table' and 'data.frame':	1 obs. of  2 variables:
##  $ x: num 1
##  $ y: num 2
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
anatomy(df)
```

```
## 'data.table', 'data.frame' with 1 row and 2 columns:
##   $ x: num  1
##   $ y: num  2
## - attr(*, ".internal.selfref")=<externalptr>
```


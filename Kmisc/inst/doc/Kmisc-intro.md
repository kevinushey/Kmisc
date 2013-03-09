<!--
%\VignetteEngine{knitr}
%\VignetteIndexEntry{Introduction to Kmisc}
\usepackage[utf8]{inputenc}
-->

<link rel="stylesheet" href="markdown.css">

Quick Introduction to Kmisc
=====

Kmisc introduces a bunch of utility functions to make the R coding experience
a bit easier. Some of the most useful functions in the package are shown here.


```r
set.seed(123)
library(Kmisc)
```

```
## Loading required package: Rcpp
```

```r
library(lattice)
library(grid)
dat <- data.frame( x=letters[1:4], y=1:4, z=LETTERS[1:4] )
```


`without`: This function is used to remove columns from a `list` / `data.frame`.


```r
## let's remove columns 'x' and 'z' from dat.
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
## I always find that syntax awkward. Let's use Kmisc::without instead.
without( dat, x, z )
```

```
##   y
## 1 1
## 2 2
## 3 3
## 4 4
```

```r
## what if there is a variable 'x' in the global environment?
x <- "a"
without( dat, x, z )
```

```
##   y
## 1 1
## 2 2
## 3 3
## 4 4
```

```r
## finally, we can also index by $ (note: NOT done by partial matching):
without( dat, dat$x, dat$z )
```

```
##   y
## 1 1
## 2 2
## 3 3
## 4 4
```

```r
## this could be handy for vectors with very long names, if using an IDE with
## auto-complete
```


`extract`: Extract vectors from a data.frame or list. Although there is already
a good subsetting syntax for lists and vectors, I wanted a complementary
function for `without`.


```r
extract( dat, x, y)
```

```
##   x y
## 1 a 1
## 2 b 2
## 3 c 3
## 4 d 4
```


`without.re, extract.re`: Extract variables whose names don't match / do match
a regular expression pattern.

```r
extract.re( dat, "[xy]")
```

```
##   x y
## 1 a 1
## 2 b 2
## 3 c 3
## 4 d 4
```

```r
without.re( dat, "[xy]")
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
##                x       y       z
## Min.    -2.31000 -2.0500 -2.0100
## 1st Qu. -0.62600 -0.7280 -0.5740
## Median  -0.05870 -0.2060  0.0535
## Mean    -0.00979 -0.0238  0.1010
## 3rd Qu.  0.56200  0.5720  0.7790
## Max.     2.19000  3.2400  2.2900
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
x <- runif(10); lo <- 0.5; hi <- 1
print( data.frame( x=x, inner=in_interval(x, lo, hi) ) )
```

```
##          x inner
## 1  0.90915  TRUE
## 2  0.05638 FALSE
## 3  0.50291  TRUE
## 4  0.35054 FALSE
## 5  0.84556  TRUE
## 6  0.80644  TRUE
## 7  0.11733 FALSE
## 8  0.71269  TRUE
## 9  0.23527 FALSE
## 10 0.07496 FALSE
```


`stack_list`: Use this to stack data.frames in a list. This can be useful if
e.g. you've run some kind of bootstrap procedure and have all your results
stored in as a list of data.frames -- even `do.call( rbind, dfs )` can be slow.
The difference is even more prominent when used on very large lists.


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
##   0.007   0.000   0.007
```

```r
system.time( do.call(rbind, dfs) )
```

```
##    user  system elapsed 
##   0.274   0.008   0.281
```


Fast String Operations
-----

For whatever reason, R is missing some nice builtin 'string' functions. I've
introduced a few functions, implemented in C for speed, for two common
string operations.

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


File I/O
-----

Sometimes, you get really large data files that just aren't going to fit into
RAM. You really wish you could split them up in a structured way, transform
them in some way, and then put them back together. One might consider a more
'enterprise' edition of the split-apply-combine framework (Hadoop and friends),
but the alternative is to use C++ to munge through a text file and pull out
things that we actually want.

`split_file`: This function splits a delimited file into multiple files, according to
unique entries in a chosen column.

`extract_rows_from_file`: From a delimited text file, extract only the rows for
which the entries in a particular column match some set of items that you
wish to keep.

C++ Function Generators
-----

These functions wrap around generated source code to produce useful, fast
functions very quickly.

`Rcpp_tapply_generator`: See the example.

```r
dat <- data.frame( y=rnorm(100), x=sample(letters[1:5], 100, TRUE) )
tMean <- Rcpp_tapply_generator("return mean(x);")
```

```
## C++ source code will be written to /var/folders/m7/_xnnz_b53kjgggkb1drc1f8c0000gn/T//RtmpFnGqLp/file44d2360cbe17.cpp .
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


`Rcpp_apply_generator`: An apply function generator tailored to 2D matrices.

```r
aMean <- Rcpp_apply_generator("return mean(x);")
```

```
## C++ source code will be written to /var/folders/m7/_xnnz_b53kjgggkb1drc1f8c0000gn/T//RtmpFnGqLp/file44d22366379b.cpp .
## Compiling...
## Done!
```

```r
mat <- matrix( rnorm(100), nrow=10 )
aMean(mat, 2)
```

```
##  [1] -0.565927 -0.759320 -0.420093  0.056437  0.303959 -0.456341  0.295674
##  [8] -0.006245 -0.690274 -0.529645
```

```r
apply(mat, 2, mean)
```

```
##  [1] -0.565927 -0.759320 -0.420093  0.056437  0.303959 -0.456341  0.295674
##  [8] -0.006245 -0.690274 -0.529645
```


Faster Common Operations through C and C++
-----

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
##        a        b        c        d        e 
## -0.02844  0.02682  0.04725  0.03514 -0.04972
```

```r
tapply_(y, x, mean)
```

```
##        a        b        c        d        e 
## -0.02844  0.02682  0.04725  0.03514 -0.04972
```

```r
microbenchmark(
  tapply(y, x, mean),
  tapply_(y, x, mean),
  tMean(y, x)
  )
```

```
## Unit: microseconds
##                  expr    min     lq median     uq    max
## 1 tapply_(y, x, mean)  75.31  78.10  79.93  82.19 1083.3
## 2  tapply(y, x, mean) 449.23 457.49 464.52 478.92 1448.0
## 3         tMean(y, x)  64.84  67.49  69.72  72.23  116.8
```


`melt_`: This function operates like `reshape2:::melt`, but works almost
entirely through the R API and hence is much faster. However, we're limited
to stacking only one final 'value' vector.

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
## 1  A -1.68462 -1.6016 -0.1789
## 2  B  0.07443  1.3885 -2.6993
## 3  C  0.68199  0.5719 -0.1031
## 4  D  0.30652  1.0557 -1.6084
## 5  E  0.80863  0.8882 -0.1994
```

```r
melt_(dat, id.vars="id")
```

```
## Warning: factors coerced to characters
```

```
##    id names    value
## 1   A    x1 -1.68462
## 2   B    x1  0.07443
## 3   C    x1  0.68199
## 4   D    x1  0.30652
## 5   E    x1  0.80863
## 6   A    x2 -1.60164
## 7   B    x2  1.38850
## 8   C    x2  0.57186
## 9   D    x2  1.05565
## 10  E    x2  0.88817
## 11  A    x3 -0.17885
## 12  B    x3 -2.69932
## 13  C    x3 -0.10313
## 14  D    x3 -1.60841
## 15  E    x3 -0.19938
```

The cool R Markdown / HTML Stuff
-----

Next, a set of helper HTML generating functions.
I've found them especially useful in laying out R Markdown documents,
and also controlling more finely CSS styling and such.

`makeHTMLTable`: Converts a `data.frame` or `matrix` into an HTML table.
Best seen with an example. Note that we require `results='asis'` in the chunk
options.


```r
dat <- data.frame(apple = c("a", "b", "c"), banana = c(1, 2, 3))
makeHTMLTable(dat, use.col.names = TRUE)
```

<table ><tr><td>apple</td><td>banana</td></tr><tr><td>a</td><td>1</td></tr><tr><td>b</td><td>2</td></tr><tr><td>c</td><td>3</td></tr></table> 


`kTable`: Generated 1D and 2D (contingency) tables, which are then typically
passed through `makeHTMLtable` for 1D tables, or `pxt` for 2D tables.


```r
x <- factor( rbinom(100, 2, 0.2) )
y <- factor( rbinom(100, 3, 0.3) )

p1t( kTable( x, top.left.cell="foo" ) )
```

<table class='oneDtable' ><tr><td >foo</td><td >Count (%)</td></tr><tr><td >0</td><td >72 (72.0%)</td></tr><tr><td >1</td><td >24 (24.0%)</td></tr><tr><td >2</td><td > 4 (4.00%)</td></tr><tr><td >Total</td><td >100</td></tr></table> 

```r
pxt( kTable(x, y, 
            top.left.cell="foo", 
            top.label="bar", 
            left.label="baz" 
            ) )
```

<table class='twoDtable' ><tr><td colspan=2 rowspan=2 >foo</td><td colspan=4 >bar</td><td ></td></tr><tr><td >0</td><td >1</td><td >2</td><td >3</td><td >Total</td></tr><tr><td rowspan=3 >baz</td><td >0</td><td >23 (79.3%)</td><td >37 (75.5%)</td><td >9 (52.9%)</td><td >3 (60.0%)</td><td >72</td></tr><tr><td >1</td><td > 5 (17.2%)</td><td >10 (20.4%)</td><td >7 (41.1%)</td><td >2 (40.0%)</td><td >24</td></tr><tr><td >2</td><td > 1 (3.44%)</td><td > 2 (4.08%)</td><td >1 (5.88%)</td><td >0 (0.00%)</td><td >4</td></tr><tr><td ></td><td >Total</td><td >29</td><td >49</td><td >17</td><td >5</td><td >100</td></tr></table> 


`kTable` with `google=TRUE`: Generate a 1D table output with the `googleVis`
package.


```r
suppressPackageStartupMessages( library(googleVis) )
tmp <- gvisTable( kTable(x, google=TRUE) )
cat( tmp$html$chart )
```

<!-- Table generated in R 2.15.3 by googleVis 0.3.3 package -->
<!-- Sat Mar  9 14:00:31 2013 -->


<!-- jsHeader -->
<script type="text/javascript" src="http://www.google.com/jsapi">
</script>
<script type="text/javascript">
 
// jsData 
function gvisDataTableID44d22c0d7d10 ()
{
  var data = new google.visualization.DataTable();
  var datajson =
[
 [
 "0",
"72 (72.0%)" 
],
[
 "1",
"24 (24.0%)" 
],
[
 "2",
" 4 (4.00%)" 
],
[
 "Total",
"100" 
] 
];
data.addColumn('string','x');
data.addColumn('string','Count (%)');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartTableID44d22c0d7d10() {
  var data = gvisDataTableID44d22c0d7d10();
  var options = {};
options["allowHtml"] = true;

     var chart = new google.visualization.Table(
       document.getElementById('TableID44d22c0d7d10')
     );
     chart.draw(data,options);
    

}
  
 
// jsDisplayChart 
function displayChartTableID44d22c0d7d10()
{
  google.load("visualization", "1", { packages:["table"] }); 
  google.setOnLoadCallback(drawChartTableID44d22c0d7d10);
}
 
// jsChart 
displayChartTableID44d22c0d7d10()
 
<!-- jsFooter -->  
//-->
</script>
 
<!-- divChart -->
  
<div id="TableID44d22c0d7d10"
  style="width: 600px; height: 500px;">
</div>


`hImg, hSvg`: These utility functions do the work of simultaneously writing a
plot to file, and then producing the HTML code needed to embed that plot in
the page. Once again, we require `results='asis'` in the chunk options. I
also show off the nice little `gradient` function.


```r
x <- rnorm(100); y <- x + runif(100)
## note that the default resolution is high for PNGs
hImg( file="test.png", width=400, height=300, dpi=300,
      xyplot( y ~ x, col=gradient(x+y) )
      )
```

<img class="center" src="test.png" width=400 height=300 />

```r
hSvg( file="test.svg", width=400, height=300,
      xyplot( y ~ x, col=gradient(x+y) )
      )
```

<div align='center'>
<embed src="test.svg" width=400 height=300 type="image/svg+xml" />
</div>


`kCoef, kAnova`: These are functions that produce 'nicer' coefficient output
from a model fit, especially when factors are used. However, they don't work
quite right for models with interaction effects yet.


```r
x <- rnorm(100); y <- x + runif(100)
z <- factor( rep(c('a','b','c','d'), each=25) )

myFit <- lm( y ~ x + z )

## the vanilla way
coef( summary( myFit ) )
```

```
##             Estimate Std. Error t value  Pr(>|t|)
## (Intercept)  0.56425    0.06421  8.7870 6.453e-14
## x            0.99799    0.02905 34.3554 2.258e-55
## zb          -0.05407    0.09057 -0.5969 5.520e-01
## zc           0.03466    0.09080  0.3817 7.035e-01
## zd          -0.09670    0.09064 -1.0668 2.887e-01
```

```r

## nicer parsing of variable names
kCoef( myFit )
```

```
##             Estimate Std. Error t value  Pr(>|t|)
## (Intercept)  0.56425    0.06421  8.7870 6.453e-14
## x            0.99799    0.02905 34.3554 2.258e-55
## z: a -> b   -0.05407    0.09057 -0.5969 5.520e-01
## z: a -> c    0.03466    0.09080  0.3817 7.035e-01
## z: a -> d   -0.09670    0.09064 -1.0668 2.887e-01
```


`html`: Custom HTML in an R Markdown document.


```r
html(
     table( class="my-favourite-table",
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

<table class='my-favourite-table'><tr><td>Apples</td><td>Bananas</td></tr><tr><td>20</td><td>30</td></tr></table>


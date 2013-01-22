<link rel="stylesheet" href="markdown.css">

Quick Introduction to Kmisc
=====

Kmisc introduces a bunch of utility functions to make the R coding experience
a bit easier. Some of the most useful functions in the package are shown here.

*Please forgive the vanity / very short namespace-like construct with many 
names, in which I prepend function names with `k`.*

Install the package with

    install.packages("Kmisc")


```r
set.seed(123)
suppressPackageStartupMessages( library(Kmisc) )
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
## 'without' looks within the object first. however...
a <- c("x", "z")
without( dat, a )
```

```
##   x y z
## 1 a 1 A
## 2 b 2 B
## 3 c 3 C
## 4 d 4 D
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


`dapply`: The `data.frame` version of the `l/sapply` series of functions.

Why have this function when `sapply` still does much the same? I always get
frustrated with the fact that either an `array` or a `list` is returned
by sapply, but never a `data.frame` .


```r

dat <- data.frame( x = rnorm(100), y = rnorm(100), z = rnorm(100) )
dapply( dat, summary )
```

```
##               x      y       z
## Min.    -2.3100 -2.050 -1.7600
## 1st Qu. -0.4940 -0.801 -0.5310
## Median   0.0618 -0.226  0.0359
## Mean     0.0904 -0.108  0.1200
## 3rd Qu.  0.6920  0.468  0.7640
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
##   id x       y       z
## 1  1 b -0.4372  0.3312
## 2  2 b -1.0525 -2.0142
## 3  3 b -0.9385      NA
## 4  4 a -0.7527  0.2120
## 5  5 a -0.7152      NA
```

```r
## even the sort parameter can't save you
merge( dat1, dat2, by="id", all.x=TRUE, sort=TRUE )
```

```
##   id x       y       z
## 1  1 b -0.4372  0.3312
## 2  2 b -1.0525 -2.0142
## 3  3 b -0.9385      NA
## 4  4 a -0.7527  0.2120
## 5  5 a -0.7152      NA
```

```r
# kMerge keeps it as is
kMerge( dat1, dat2, by="id" )
```

```
##   id x       y       z
## 5  5 a -0.7152      NA
## 4  4 a -0.7527  0.2120
## 3  3 b -0.9385      NA
## 2  2 b -1.0525 -2.0142
## 1  1 b -0.4372  0.3312
```


`in_interval`: A fast C implementation for determing which elements of a 
vector `x` lie within an interval `[lo, hi)`.


```r
x <- runif(10); lo <- 0.5; hi <- 1
print( data.frame( x=x, inner=in_interval(x, lo, hi) ) )
```

```
##          x inner
## 1  0.89190  TRUE
## 2  0.75487  TRUE
## 3  0.97920  TRUE
## 4  0.04415 FALSE
## 5  0.90340  TRUE
## 6  0.86550  TRUE
## 7  0.77541  TRUE
## 8  0.37682 FALSE
## 9  0.04211 FALSE
## 10 0.36441 FALSE
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
str_rev2( c("はひふへほ") )
```

```
## [1] "ほへふひは"
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
str_slice2( "ははひひふふへへほほ", 2 )
```

```
## Error: values must be length 1, but FUN(X[[1]]) result is length 5
```


`str_sort`: sort a string. Doesn't handle unicode strings.

```r
str_sort(c("はおいれ", "asnoighewgypfuiweb"))
```

```
## [1] "いおはれ"           "abeefgghiinopsuwwy"
```


The cool R Markdown / HTML Stuff
-----

The real point of releasing this package is due to the HTML helper functions
I've made. I've found them especially useful in laying out R Markdown documents,
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

<table class='oneDtable' ><tr><td >foo</td><td >Count (%)</td></tr><tr><td >0</td><td >65 (65.0%)</td></tr><tr><td >1</td><td >34 (34.0%)</td></tr><tr><td >2</td><td > 1 (1.00%)</td></tr><tr><td >Total</td><td >100</td></tr></table> 

```r
pxt( kTable(x, y, 
            top.left.cell="foo", 
            top.label="bar", 
            left.label="baz" 
            ) )
```

<table class='twoDtable' ><tr><td colspan=2 rowspan=2 >foo</td><td colspan=4 >bar</td><td ></td></tr><tr><td >0</td><td >1</td><td >2</td><td >3</td><td >Total</td></tr><tr><td rowspan=3 >baz</td><td >0</td><td >26 (66.6%)</td><td >26 (66.6%)</td><td >12 (63.1%)</td><td >1 (33.3%)</td><td >65</td></tr><tr><td >1</td><td >13 (33.3%)</td><td >12 (30.7%)</td><td > 7 (36.8%)</td><td >2 (66.6%)</td><td >34</td></tr><tr><td >2</td><td > 0 (0.00%)</td><td > 1 (2.56%)</td><td > 0 (0.00%)</td><td >0 (0.00%)</td><td >1</td></tr><tr><td ></td><td >Total</td><td >39</td><td >39</td><td >19</td><td >3</td><td >100</td></tr></table> 


`kTable` with `google=TRUE`: Generate a 1D table output with the `googleVis`
package.


```r
suppressPackageStartupMessages( library(googleVis) )
tmp <- gvisTable( kTable(x, google=TRUE) )
cat( tmp$html$chart )
```

<!-- Table generated in R 2.15.2 by googleVis 0.3.3 package -->
<!-- Mon Jan 21 14:34:16 2013 -->


<!-- jsHeader -->
<script type="text/javascript" src="http://www.google.com/jsapi">
</script>
<script type="text/javascript">
 
// jsData 
function gvisDataTableID276d3bba736a ()
{
  var data = new google.visualization.DataTable();
  var datajson =
[
 [
 "0",
"65 (65.0%)" 
],
[
 "1",
"34 (34.0%)" 
],
[
 "2",
" 1 (1.00%)" 
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
function drawChartTableID276d3bba736a() {
  var data = gvisDataTableID276d3bba736a();
  var options = {};
options["allowHtml"] = true;

     var chart = new google.visualization.Table(
       document.getElementById('TableID276d3bba736a')
     );
     chart.draw(data,options);
    

}
  
 
// jsDisplayChart 
function displayChartTableID276d3bba736a()
{
  google.load("visualization", "1", { packages:["table"] }); 
  google.setOnLoadCallback(drawChartTableID276d3bba736a);
}
 
// jsChart 
displayChartTableID276d3bba736a()
 
<!-- jsFooter -->  
//-->
</script>
 
<!-- divChart -->
  
<div id="TableID276d3bba736a"
  style="width: 600px; height: 500px;">
</div>


`hImg, hSvg`: These utility functions do the work of simultaneously writing a
plot to file, and then producing the HTML code needed to embed that plot in
the page. Once again, we require `results='asis'` in the chunk options. I
also show off the nice little `kColor` function.


```r
x <- rnorm(100); y <- x + runif(100)
## note that the default resolution is high for PNGs
hImg( file="test.png", width=400, height=300, dpi=72,
      xyplot( y ~ x, col=kColor(x+y) )
      )
```

```
## Error: could not find function "kColor"
```

```r
hSvg( file="test.svg", width=400, height=300,
      xyplot( y ~ x, col=kColor(x+y) )
      )
```

```
## Error: could not find function "kColor"
```


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
## (Intercept)  0.49270    0.05457  9.0282 1.973e-14
## x            1.04476    0.02700 38.6959 5.885e-60
## zb          -0.11483    0.07717 -1.4880 1.401e-01
## zc          -0.01759    0.07724 -0.2277 8.203e-01
## zd           0.01866    0.07751  0.2408 8.103e-01
```

```r

## nicer parsing of variable names
kCoef( myFit )
```

```
##             Estimate Std. Error t value  Pr(>|t|)
## (Intercept)  0.49270    0.05457  9.0282 1.973e-14
## x            1.04476    0.02700 38.6959 5.885e-60
## z: a -> b   -0.11483    0.07717 -1.4880 1.401e-01
## z: a -> c   -0.01759    0.07724 -0.2277 8.203e-01
## z: a -> d    0.01866    0.07751  0.2408 8.103e-01
```


`html`: Custom HTML in an R Markdown document. We can load a bunch of utility
functions for HTML markup with `data(html)`; the functions are all hidden
inside that environment so as to not gum up the main namespace. You could,
of course, attach them if you wanted, with `attach(html)`, but you would
then be masking functions like `table`.


```r
data(html)
with(html,
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


`attachHTML, detachHTML`: If you want to avoid using the `with` syntax, you can also load a
namespace of 'common' HTML functions, or just generate your own.


```r
attachHTML()
span( style="color: red;",
      "This is some red text."
      )
```

<span style='color: red;'>This is some red text.</span>

```r
article <- Kmisc:::makeHTMLTag("article")
article( class="class", id="id", p("Some text") )
```

<article class='class' id='id'><p>Some text</p></article>

```r
detachHTML()
```


<link rel="stylesheet" href="markdown.css">

Quick Introduction to Kmisc
=====

Kmisc introduces a bunch of utility functions to make the R coding experience
a bit easier. Some of the most useful functions in the package are shown here.

*Please forgive the vanity / very short namespace-like construct with many names,
in which I prepend function names with `k`.*


```r
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


`kReplace`: Replace elements in a vector.


```r
tDat <- dat ## make a temporary copy of dat

## Replace some elements in tDat$y
tDat$y <- kReplace( tDat$y, from=c(4, 8, 10), to=c(20, 40, 60) )
cbind( dat$y, tDat$y )
```

```
##      [,1] [,2]
## [1,]    1    1
## [2,]    2    2
## [3,]    3    3
## [4,]    4   20
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
##                x       y       z
## Min.    -2.56000 -2.8000 -3.3800
## 1st Qu. -0.62400 -0.5800 -0.5660
## Median   0.07340 -0.0262  0.1320
## Mean    -0.00764  0.0822 -0.0122
## 3rd Qu.  0.75900  0.9270  0.7110
## Max.     2.09000  2.2100  2.4800
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
##   id x      y       z
## 1  1 b -0.245  0.5016
## 2  2 b -0.708 -1.6663
## 3  3 b -1.285      NA
## 4  4 a  1.136 -0.3671
## 5  5 a -2.036      NA
```

```r
## even the sort parameter can't save you
merge( dat1, dat2, by="id", all.x=TRUE, sort=TRUE )
```

```
##   id x      y       z
## 1  1 b -0.245  0.5016
## 2  2 b -0.708 -1.6663
## 3  3 b -1.285      NA
## 4  4 a  1.136 -0.3671
## 5  5 a -2.036      NA
```

```r
# kMerge keeps it as is
kMerge( dat1, dat2, by="id" )
```

```
##   id x      y       z
## 5  5 a -2.036      NA
## 4  4 a  1.136 -0.3671
## 3  3 b -1.285      NA
## 2  2 b -0.708 -1.6663
## 1  1 b -0.245  0.5016
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

<table class='oneDtable' ><tr><td >foo</td><td >Count (%)</td></tr><tr><td >0</td><td >67 (67.0%)</td></tr><tr><td >1</td><td >31 (31.0%)</td></tr><tr><td >2</td><td > 2 (2.00%)</td></tr><tr><td >Total</td><td >100</td></tr></table> 

```r
pxt( kTable(x, y, 
            top.left.cell="foo", 
            top.label="bar", 
            left.label="baz" 
            ) )
```

<table class='twoDtable' ><tr><td colspan=2 rowspan=2 >foo</td><td colspan=4 >bar</td><td ></td></tr><tr><td >0</td><td >1</td><td >2</td><td >3</td><td >Total</td></tr><tr><td rowspan=3 >baz</td><td >0</td><td >21 (60.0%)</td><td >33 (76.7%)</td><td >12 (57.1%)</td><td >1 ( 100%)</td><td >67</td></tr><tr><td >1</td><td >13 (37.1%)</td><td > 9 (20.9%)</td><td > 9 (42.8%)</td><td >0 (0.00%)</td><td >31</td></tr><tr><td >2</td><td > 1 (2.85%)</td><td > 1 (2.32%)</td><td > 0 (0.00%)</td><td >0 (0.00%)</td><td >2</td></tr><tr><td ></td><td >Total</td><td >35</td><td >43</td><td >21</td><td >1</td><td >100</td></tr></table> 


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

<img class="center" src="test.png" width=400 height=300 />

```r
hSvg( file="test.svg", width=400, height=300,
      xyplot( y ~ x, col=kColor(x+y) )
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
## (Intercept)  0.41616    0.06167  6.7486 1.171e-09
## x            0.97947    0.03082 31.7848 2.033e-52
## zb           0.12491    0.08590  1.4540 1.492e-01
## zc           0.09850    0.08636  1.1405 2.569e-01
## zd           0.04536    0.08571  0.5293 5.979e-01
```

```r

## nicer parsing of variable names
kCoef( myFit )
```

```
##             Estimate Std. Error t value  Pr(>|t|)
## (Intercept)  0.41616    0.06167  6.7486 1.171e-09
## x            0.97947    0.03082 31.7848 2.033e-52
## z: a -> b    0.12491    0.08590  1.4540 1.492e-01
## z: a -> c    0.09850    0.08636  1.1405 2.569e-01
## z: a -> d    0.04536    0.08571  0.5293 5.979e-01
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


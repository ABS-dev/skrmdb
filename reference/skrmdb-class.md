# skrmdb class

The functions
[`ReedMuench()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
[`SpearKarb()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
[`DragBehr()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
and
[`skrmdb.all()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md)
return an S3 object of class `skrmdb`.

The `skrmdb` class object contains the following fields:

- `eval`: The name of the method used to compute the ED50.

- `data`: A `data.frame` of the data used to compute the ED50.

- `ed`: A vector of ED50 values.

- `var`: A vector of variances, only is SpearKarb was used.

- `results`: A `data.table` of the results, including brief information
  about the data.

These components can be accessed via the appropriate accessor functions.

## Usage

``` r
getED50(x)

getVar(x)

getData(x)

getResults(x)
```

## Arguments

- x:

  An object of class `skrmdb`.

## Examples

``` r
y <- c(0, 3, 5, 8, 10, 10)
n <- rep(10, 6)
x <- 1:6
res <- SpearKarb(y + n ~ x)
#> skrmdb :: Autosorting dilution sequences.
print(res)
#> ED50 by the Spearman-Karber method.
#> 
#>   ed   var  Duplicate Dilutions  Response  Monotonic Bracket
#> 2.900 0.069     No     Regular  Increasing    Yes      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to increase as x increases.
#> ✔ All count trends are monotonic along x.
#> ✔ All count trends bracket ED50.
#>  
getED50(res)
#> [1] 2.9
getVar(res)
#> [1] 0.06888889
getData(res)
#>    y  n x y_inc y_dec Duplicate Dilutions   Response Monotonic Bracket
#> 1  0 10 1     0    10        No   Regular Increasing       Yes     Yes
#> 2  3 10 2     3     7        No   Regular Increasing       Yes     Yes
#> 3  5 10 3     5     5        No   Regular Increasing       Yes     Yes
#> 4  8 10 4     8     2        No   Regular Increasing       Yes     Yes
#> 5 10 10 5    10     0        No   Regular Increasing       Yes     Yes
#> 6 10 10 6    10     0        No   Regular Increasing       Yes     Yes
getResults(res)
#>    ed        var Duplicate Dilutions   Response Monotonic Bracket
#> 1 2.9 0.06888889        No   Regular Increasing       Yes     Yes
```

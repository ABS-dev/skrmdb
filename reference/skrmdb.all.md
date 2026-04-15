# Function to run all three methods for determining ED50.

This function is used primarily by the CVB statistics section to run all
three methods included in the
[skrmdb](https://abs-dev.github.io/skrmdb/reference/skrmdb.md) package
for determining ED50.

## Usage

``` r
skrmdb.all(formula, data, autosort = TRUE)
```

## Arguments

- formula:

  A formula of the form `y + n ~ x` or `y + n ~ x | v1 + ... + vK`. `y`
  is the number responding at each dilution level, `n` is the number
  tested at each dilution level, and `x` is the dilution levels. `v1`,
  ..., `vK` are the grouping variables. `y`, `n`, `x`, `v1`, ..., `vK`
  must all be distinct.

- data:

  A data.frame containing the titration data. Formatted as specified in
  the CVB Data Guide.

- autosort:

  Default `TRUE`. If `TRUE` will sort the data according to either
  `sort(x)` or `sort(-x)` so that `y / n` appears to be increasing with
  the index. This is how the three methods assume the data to be
  ordered.

## Value

A `data.frame` containing columns for each of the subset variables, the
ED50 as computed by `DragBehr`, `ReedMuench`, the ED50 and variance as
computed by `SpearKarb`, and columns of logical values which indicate if
the data are increasing or decreasing with `log_dil`, has an even
dilution scheme, are monotonic, and brackets the midpoint.

## Examples

``` r
data(titration)
titration$log_dil = -log10(titration$dil)
skrmdb.all(positive + total ~ log_dil | Vial + Operator, titration)
#>   Vial Operator DragBehr ReedMuench SpearKarb SpearKarb.var response.increasing
#> 1    1       TK 5.306306   5.266667      5.30    0.06666667               FALSE
#> 2    1       NU 5.429825   5.411765      5.40    0.04777778               FALSE
#> 3    1       CT       NA         NA      5.20    0.02777778               FALSE
#> 4    2       TK 5.185185   5.153846      5.20    0.04555556               FALSE
#> 5    2       NU 5.285714   5.235294      5.20    0.02333333               FALSE
#> 6    2       CT 5.500000   5.500000      5.50    0.03555556               FALSE
#> 7    3       TK 5.482759   5.400000      5.55    0.08250000               FALSE
#> 8    3       NU 5.411765   5.375000      5.40    0.04333333               FALSE
#> 9    3       CT 5.349462   5.312500      5.30    0.03333333               FALSE
#>   duplicate.dilutions even.dilution monotonic bracket.midpoint
#> 1               FALSE          TRUE     FALSE             TRUE
#> 2               FALSE          TRUE     FALSE             TRUE
#> 3               FALSE          TRUE     FALSE            FALSE
#> 4               FALSE          TRUE      TRUE             TRUE
#> 5               FALSE          TRUE      TRUE             TRUE
#> 6               FALSE          TRUE      TRUE             TRUE
#> 7               FALSE         FALSE      TRUE             TRUE
#> 8               FALSE          TRUE      TRUE             TRUE
#> 9               FALSE          TRUE      TRUE             TRUE

titration$dil = NULL
skrmdb.all(positive + total ~ log_dil | ., titration)
#>   testID   PrepID PrepRole      Date Vial Operator DragBehr ReedMuench
#> 1   BRSV BRSV-001     test 03-Mar-19    1       TK 5.306306   5.266667
#> 2   BRSV BRSV-001     test 03-Mar-19    1       NU 5.429825   5.411765
#> 3   BRSV BRSV-001     test 03-Mar-19    1       CT       NA         NA
#> 4   BRSV BRSV-001     test 03-Mar-19    2       TK 5.185185   5.153846
#> 5   BRSV BRSV-001     test 03-Mar-19    2       NU 5.285714   5.235294
#> 6   BRSV BRSV-001     test 03-Mar-19    2       CT 5.500000   5.500000
#> 7   BRSV BRSV-001     test 03-Mar-19    3       TK 5.482759   5.400000
#> 8   BRSV BRSV-001     test 03-Mar-19    3       NU 5.411765   5.375000
#> 9   BRSV BRSV-001     test 03-Mar-19    3       CT 5.349462   5.312500
#>   SpearKarb SpearKarb.var response.increasing duplicate.dilutions even.dilution
#> 1      5.30    0.06666667               FALSE               FALSE          TRUE
#> 2      5.40    0.04777778               FALSE               FALSE          TRUE
#> 3      5.20    0.02777778               FALSE               FALSE          TRUE
#> 4      5.20    0.04555556               FALSE               FALSE          TRUE
#> 5      5.20    0.02333333               FALSE               FALSE          TRUE
#> 6      5.50    0.03555556               FALSE               FALSE          TRUE
#> 7      5.55    0.08250000               FALSE               FALSE         FALSE
#> 8      5.40    0.04333333               FALSE               FALSE          TRUE
#> 9      5.30    0.03333333               FALSE               FALSE          TRUE
#>   monotonic bracket.midpoint
#> 1     FALSE             TRUE
#> 2     FALSE             TRUE
#> 3     FALSE            FALSE
#> 4      TRUE             TRUE
#> 5      TRUE             TRUE
#> 6      TRUE             TRUE
#> 7      TRUE             TRUE
#> 8      TRUE             TRUE
#> 9      TRUE             TRUE
```

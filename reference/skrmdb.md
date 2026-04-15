# skrmdb package

The skrmdb package provides functionality to compute the median
effective dose (ED50) using the Dragstedt-Behrens, Reed-Muench, and
Spearman-Kärber estimators.

## Usage

``` r
DragBehr(formula, data, y, n, x, autosort = TRUE, warn.me = TRUE, show = FALSE)

ReedMuench(
  formula,
  data,
  y,
  n,
  x,
  autosort = TRUE,
  warn.me = TRUE,
  show = FALSE
)

SpearKarb(
  formula,
  data,
  y,
  n,
  x,
  autosort = TRUE,
  warn.me = TRUE,
  show = FALSE
)
```

## Arguments

- formula:

  a formula of the form `y + n ~ x` or `cbind(y, n) ~ x`

- data:

  a data frame

- y:

  an integer vector corresponding to the number responding at each log
  dilution or dose.

- n:

  an integer vector corresponding to the group size at each log dilution
  or dose.

- x:

  a vector corresponding to the log dilution or dose for each group.

- autosort:

  Default `TRUE`. If `TRUE` will sort the data according to either
  `sort(x)` or `sort(-x)` so that `y / n` appears to be increasing with
  the index. This is how the three methods assume the data to be
  ordered.

- warn.me:

  if TRUE, warnings and messages related to the processing of the data
  will be displayed.

- show:

  if TRUE, will print the intermediary statistics used to calculate
  ED50.

## Value

An object of class
[skrmdb](https://abs-dev.github.io/skrmdb/reference/skrmdb-class.md)

## Details

The Dragstedt-Behrens and Reed-Muench methods estimate the median
effective dose by interpolating between the two doses that bracket the
dose producing median response. They accumulate sums in both directions
by assuming that those that responded at a lower dose would respond at a
higher dose, and those that did not respond at a higher dose would not
respond at a lower dose. The Dragstedt-Behrens method estimates ED50 by
interpolating on the line that connects the hypothetical fractions of
the bracketing doses for ED50, while the Reed-Muench method estimates
ED50 as the intersection of the lines connecting the two sets of
cumulative sums between bracketing doses.

The Spearman-Karber method gives a non-parametric estimate of the mean
of an tolerance distribution from its empirical distribution (EDF). The
empirical PMF is derived from the EDF by differencing and the estimator
is \\\sum{ x f(x)}\\. If the EDF does not cover the entire support of
`x`, `SpearKarb()` extends it by assuming the next lower dilution would
produce zero response and the next higher dilution would produce
complete response.

## Note

These methods assume that the `y` is monotonic in `x`, however ED50 will
still be computed if this is not the case. These methods also assume
that data brackets ED50. If the data does not bracket ED50, a result
could still be returned, but the accuracy of this value in estimating
ED50 is suspect.

Many microbiology texts mistakenly present the Dragstedt-Behrens method
as the Reed-Muench method.

## References

Behrens, B. (1929) Zur Auswertung der Digitalisblätter im Froschversuch.
*Arkiv für Experimentelle Pathologie und Pharmakologie.* **140:
237-256**.

Dragstedt, C. A., Lang, V. F. (1928). Respiratory Stimulants in Acute
Cocaine Poisoning in Rabbits. *J. of Pharmacology and Experimental
Therapeutics.* **32: 215–222**.

Kärber, G. (1931). Beitrag zur kollektiven Behandlung Parmakogischer
Reihenversuche. *Archiv für Experimentelle Pathologie und
Pharmakologie.* **162: 480–483**.

Miller, Rupert G. (1973). Nonparametric Estimators of the Mean Tolerance
in Bioassay. *Biometrika.* **60: 535 - 542**.

Reed LJ, Muench H (1938). A Simple Method of Estimating Fifty Percent
Endpoints. *American Journal of Hygiene.* **27: 493–497**.

Spearman, C. (1908). The Method of "Right and Wrong Cases" ("Constant
Stimuli") without Gauss's Formulae. *Brit. J. of Psychology.* **2:
227–242**.

## See also

Useful links:

- <https://github.com/ABS-dev/skrmdb/>

- <https://abs-dev.github.io/skrmdb/>

- Report bugs at <https://github.com/ABS-dev/skrmdb/issues/>

## Author

**Maintainer**: CVB Statistics <CVB.Data.Help@usda.gov> \[former owner\]

Authors:

- David Siev (until 2019)

- Marie Vendettuoli (until 2019)

- Thomas Kent (starting 2020)

## Examples

``` r
# All examples are with `SpearKarb`, however, the usage for
# `DragBehr` and `ReedMuench` is identical.

## Monotonically increasing data
# The three calls are equivalent.
dead <- c(0, 3, 5, 8, 10, 10)
total <- rep(10, 6)
dil <- 1:6
data <- data.frame(y = dead, n = total, x = dil)
SpearKarb(dead + total ~ dil)
#> ED50 by the Spearman-Karber method
#> ed:   2.9 
#> Count trend is increasing with dilution.
SpearKarb(y + n ~ x, data)
#> ED50 by the Spearman-Karber method
#> ed:   2.9 
#> Count trend is increasing with dilution.
SpearKarb(y = dead, n = total, x = dil)  # depreciated
#> Warning: skrmdb :: Calling this function with the parameters y, n, x is depreciated.
#> ED50 by the Spearman-Karber method
#> ed:   2.9 
#> Count trend is increasing with dilution.

## Decreasing data
# The function will reverse the order of the data
# and two calls are equivalent.
dead <- c(10, 10, 8, 5, 3, 0)
total <- rep(10, 6)
dil <- 1:6
SpearKarb(dead + total ~ dil)
#> ED50 by the Spearman-Karber method
#> ed:   4.1 
#> Count trend is decreasing with dilution.
SpearKarb(rev(dead) + rev(total) ~ rev(dil))
#> ED50 by the Spearman-Karber method
#> ed:   4.1 
#> Count trend is decreasing with dilution.

## Unordered data
# Observe that the data is not monotonic after being sorted by dil.
dead <- c(10, 8, 5, 3, 0)
total <- rep(10, 5)
dil <- c(1, 3, 2, 4, 5)
SpearKarb(dead + total ~ dil)
#> skrmdb :: y is not monotonic. ED50 may be unreliable.
#> ED50 by the Spearman-Karber method
#> ed:   3.1 
#> Count trend is decreasing with dilution.
#> y is not monotonic. ED50 may be unreliable.
```

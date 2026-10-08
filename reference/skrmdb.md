# Functions for computing ED50

The skrmdb package provides functionality to compute the median
effective dose (ED50) using the Dragstedt-Behrens, Reed-Muench, and
Spearman-Kärber estimators.

The Dragstedt-Behrens and Reed-Muench methods estimate the median
effective dose by interpolating between the two doses that bracket the
dose producing median response. They accumulate sums in both directions
by assuming that subjects that responded at a lower dose would respond
at a higher dose, and subjects that did not respond at a higher dose
would not respond at a lower dose. The Dragstedt-Behrens method
estimates ED50 by interpolating on the line that connects the
hypothetical fractions of the bracketing doses for ED50, while the
Reed-Muench method estimates ED50 as the intersection of the lines
connecting the two sets of cumulative sums between bracketing doses.

The Spearman-Kärber method gives a non-parametric estimate of the mean
of a tolerance distribution from its empirical distribution (EDF). The
empirical PMF is derived from the EDF by differencing. The estimator is
\\\sum{ x f(x)}\\. If the EDF does not cover the entire support of `x`,
`SpearKarb()` extends it by assuming the next lower dilution would
produce zero response and the next higher dilution would produce
complete response.

The function `skrmdb.all()` reports the results for all three methods.

There are several assumptions that these methods make:

1.  The group size \\n_i\\ is constant.

    - Group sizes and responses are scaled silently if this condition is
      not met.

2.  The log dilutions \\\mathbf{x} = \\x_1, x_2, \dots, x_k\\\\ form an
    increasing sequence.

    - The data will be automatically sorted so this condition holds. If
      a dilution \\x\\ appears more than once in \\\mathbf{x}\\, a new
      dilution sequence will be created where each dilution appears
      once. For each dilution \\x\\, the corresponding \\n_i\\ values
      are summed to create a new \\n\\ sequence, and the corresponding
      \\y_i\\ values are summed to create a new \\y\\ sequence. The
      column **Duplicate** in the output reports "Yes" if a dilution
      appears more than once in the original sequence and "No"
      otherwise.

3.  That \\\mathbf{x}\\ is an arithmetic sequence. That is, there is
    some number \\\Delta\\ such that \\x_k = x_1 + (k - 1) \times
    \Delta\\ for all \\k\\.

    - The column **Dilutions** in the output reports "regular" if this
      condition is met and "irregular" otherwise. Generally, this is
      used to indicate the possibility of a missing dilution in a
      sequence. However, it is possible that in some cases, a sequence
      which is marked as "irregular" may in fact be regular and have no
      missing dilutions. In these cases, use your best judgment.

4.  The count ratio sequence \\\mathbf{r} = \\r_1, r_2, \dots, r_k\\\\
    defined by \\r_i = y_i / n_i\\ trends from smaller to larger.

    - The column **Response** indicates whether \\\mathbf{r}\\ is
      "increasing", "decreasing", or shows "no change". If it is
      "decreasing" and `autosort` is `TRUE`, then ED50 will be computed
      using the sequence \\1 - r_i\\, which is the appropriate
      transformation for decreasing response data. When the data is very
      noisy and the trend is not clear, it is likely that using these
      methods to compute ED50 is not appropriate.

5.  Preferably, the count ratio sequence is monotonic, that is \\r_i
    \leq r\_{i+1}\\ for all \\i\\ or \\r_i \geq r\_{i+1}\\ for all
    \\i\\.

    - The column **Monotonic** indicates if this condition holds.
      Real-world data is rarely monotonic, but good data is generally
      "close enough to monotonic for all practical purposes." Once
      again, use your best judgment.

6.  The log dilution sequence brackets ED50. That is, there is an \\i\\
    such that \\x_i \leq \text{ED50} \leq x\_{i+1}\\. Note: This is
    **not** equivalent to saying that there is an \\i\\ such that \\r_i
    \leq 0.5 \leq r\_{i + 1}\\. However, if it is the case that \\0.5 \<
    r_1\\ or \\r_k \< 0.5\\, then the log dilution sequence will not
    bracket ED50.

    - The column **Bracket** indicates if this condition holds. If it
      does not hold, use your best judgment as to whether or not ED50
      could be considered to be meaningful.

## Usage

``` r
DragBehr(
  data,
  formula,
  autosort = TRUE,
  warn.me = TRUE,
  show = FALSE,
  y = deprecated(),
  n = deprecated(),
  x = deprecated()
)

ReedMuench(
  data,
  formula,
  autosort = TRUE,
  warn.me = TRUE,
  show = FALSE,
  y = deprecated(),
  n = deprecated(),
  x = deprecated()
)

skrmdb.all(data, formula, autosort = TRUE, warn.me = TRUE)

SpearKarb(
  data,
  formula,
  autosort = TRUE,
  warn.me = TRUE,
  show = FALSE,
  y = deprecated(),
  n = deprecated(),
  x = deprecated()
)
```

## Arguments

- data:

  A `data.frame` containing the titration data. Formatted as specified
  in the CVB Data Guide.

- formula:

  A formula of the form `y + n ~ x` or `y + n ~ x | w1 + ... + wn` where
  `w1` ... `wn` are grouping variables. All variables must be distinct.

- autosort:

  If `TRUE`, the functions will compute the ED50 based on either `y / n`
  or `1 - y / n`, whichever appears to increase with `x`. This is how
  the three methods assume the data to be ordered. If `FALSE`, ED50 will
  be computed using `y / n`, which could give incorrect results. Do not
  change this parameter unless you are certain you know what you are
  doing.

- warn.me:

  If `TRUE`, warnings and messages related to the processing of the data
  will be displayed. These warnings correspond to the assumptions and
  feedback columns in the output as outlined in the **Description**
  section above.

- show:

  If `TRUE`, will print the intermediate statistics used to calculate
  ED50. These statistics are captured in the data component of the
  output.

- y:

  **\[deprecated\]** An integer vector corresponding to the number
  responding at each log dilution or dose.

- n:

  **\[deprecated\]** An integer vector corresponding to the group size
  at each log dilution or dose.

- x:

  **\[deprecated\]** A vector corresponding to the log dilution or dose
  for each group.

## Value

A list of class
[skrmdb](https://abs-dev.github.io/skrmdb/reference/skrmdb-class.md)
which contains the following elements

- `eval`: Which method or methods were used to compute ED50.

- `data`: The transformed data used to compute ED50. It contains the
  columns `y`, `n`, `x`, and any extra columns on which the results are
  conditioned. The columns `y_inc` and `y_dec` contain the increasing
  and decreasing versions of the response proportions, based on `y / n`
  and `1 - y / n`. The columns `Duplicate`, `Dilutions`, `Response`,
  `Monotonic`, and `Bracket` give information about the how well the
  data for each group meets each of the assumptions and can be used to
  find specific parts of the data which do not meet a particular
  assumption.

- `ed`: ED50. If returned from `skrmdb.all()`, then `NA`.

- `var`: The variance. Only meaningful for `SpearKarb()`.

- `results`: A `data.frame` giving the ED50 for each group based on the
  conditions. it also contains the columns `Duplicate`, `Dilutions`,
  `Response`, `Monotonic`, and `Bracket` which describe how well the
  data for that group meets each of the assumptions.

- `autosort`: The value of the parameter `autosort`.

## Note

Many microbiology texts mistakenly present the Dragstedt-Behrens method
as the Reed-Muench method.

## References

Behrens, B. (1929) Zur Auswertung der Digitalisblätter im Froschversuch.
*Arkiv für Experimentelle Pathologie und Pharmakologie.* **140:
237-256**.

Dragstedt, C. A., Lang, V. F. (1928). Respiratory Stimulants in Acute
Cocaine Poisoning in Rabbits. *J. of Pharmacology and Experimental
Therapeutics.* **32: 215–222**.

Kärber, G. (1931). Beitrag zur kollektiven Behandlung Pharmakologischer
Reihenversuche. *Archiv für Experimentelle Pathologie und
Pharmakologie.* **162: 480–483**.

Miller, Rupert G. (1973). Nonparametric Estimators of the Mean Tolerance
in Bioassay. *Biometrika.* **60: 535 - 542**.

Reed, L.J., Muench, H. (1938). A Simple Method of Estimating Fifty
Percent Endpoints. *American Journal of Hygiene.* **27: 493–497**.

Spearman, C. (1908). The Method of "Right and Wrong Cases" ("Constant
Stimuli") without Gauss's Formulae. *Brit. J. of Psychology.* **2:
227–242**.

## Examples

``` r
# Processing Data with grouping variables.

titration$log_dil <- -log10(titration$dil)

SpearKarb(titration, positive + total ~ log_dil)
#> skrmdb :: Combining results from duplicate dilutions.
#> ED50 by the Spearman-Karber method.
#> 
#>   ed   var  Duplicate Dilutions  Response  Monotonic Bracket
#> 5.332 0.005    Yes     Regular  Decreasing    Yes      Yes   
#> 
#> ✖ Duplicate dilutions detected - counts combined.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to decrease as x increases. 
#>   `autosort == TRUE`: decreasing response ED50 values computed using 1-y/n.
#> ✔ All count trends are monotonic along x.
#> ✔ All count trends bracket ED50.
#>  
SpearKarb(titration, positive + total ~ log_dil | Operator)
#> skrmdb :: Combining results from duplicate dilutions.
#> skrmdb :: y is not monotonic. ED50 may be unreliable.
#> ED50 by the Spearman-Karber method.
#> 
#> Operator   ed   var  Duplicate Dilutions  Response  Monotonic Bracket
#>    CT    5.383 0.014    Yes     Regular  Decreasing     No      Yes  
#>    NU    5.333 0.012    Yes     Regular  Decreasing     No      Yes  
#>    TK    5.283 0.020    Yes     Regular  Decreasing     No      Yes   
#> 
#> ✖ Duplicate dilutions detected - counts combined.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to decrease as x increases. 
#>   `autosort == TRUE`: decreasing response ED50 values computed using 1-y/n.
#> ✖ Some count trends are not monotonic along x.
#> ✔ All count trends bracket ED50.
#>  
SpearKarb(titration, positive + total ~ log_dil | Vial)
#> skrmdb :: Combining results from duplicate dilutions.
#> ED50 by the Spearman-Karber method.
#> 
#> Vial   ed   var  Duplicate Dilutions  Response  Monotonic Bracket
#>   1  5.383 0.022    Yes     Regular  Decreasing    Yes      Yes  
#>   2  5.300 0.012    Yes     Regular  Decreasing    Yes      Yes  
#>   3  5.333 0.015    Yes     Regular  Decreasing    Yes      Yes   
#> 
#> ✖ Duplicate dilutions detected - counts combined.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to decrease as x increases. 
#>   `autosort == TRUE`: decreasing response ED50 values computed using 1-y/n.
#> ✔ All count trends are monotonic along x.
#> ✔ All count trends bracket ED50.
#>  
SpearKarb(titration, positive + total ~ log_dil | Operator + Vial)
#> skrmdb :: Possible missing dilution or irregular dilution series detected.
#> skrmdb :: y is not monotonic. ED50 may be unreliable.
#> skrmdb :: Dilutions fail to bracket the midpoint. ED50 is unreliable.
#> ED50 by the Spearman-Karber method.
#> 
#> Operator Vial   ed   var  Duplicate Dilutions  Response  Monotonic Bracket
#>    CT      1  5.200 0.028     No     Regular  Decreasing     No       No  
#>    CT      2  5.500 0.036     No     Regular  Decreasing    Yes      Yes  
#>    CT      3  5.300 0.033     No     Regular  Decreasing    Yes      Yes  
#>    NU      1  5.400 0.048     No     Regular  Decreasing     No      Yes  
#>    NU      2  5.200 0.023     No     Regular  Decreasing    Yes      Yes  
#>    NU      3  5.400 0.043     No     Regular  Decreasing    Yes      Yes  
#>    TK      1  5.300 0.067     No     Regular  Decreasing     No      Yes  
#>    TK      2  5.200 0.046     No     Regular  Decreasing    Yes      Yes  
#>    TK      3  5.550 0.082     No    Irregular Decreasing    Yes      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✖ Possible missing dilution or irregular dilution series detected.
#> ✔ All count trends appear to decrease as x increases. 
#>   `autosort == TRUE`: decreasing response ED50 values computed using 1-y/n.
#> ✖ Some count trends are not monotonic along x.
#> ✖ Some count trends do not bracket ED50. 

skrmdb.all(titration, positive + total ~ log_dil | Operator + Vial)
#> skrmdb :: Possible missing dilution or irregular dilution series detected.
#> skrmdb :: y is not monotonic. ED50 may be unreliable.
#> skrmdb :: Dilutions fail to bracket the midpoint. ED50 is unreliable.
#> ED50 by the all skrmdb methods.
#> 
#> Operator Vial DragBehr ReedMuench SpearKarb SpearKarb.var Duplicate Dilutions  Response  Monotonic Bracket
#>    CT      1     NA        NA       5.200       0.028         No     Regular  Decreasing     No       No  
#>    CT      2    5.500     5.500     5.500       0.036         No     Regular  Decreasing    Yes      Yes  
#>    CT      3    5.349     5.312     5.300       0.033         No     Regular  Decreasing    Yes      Yes  
#>    NU      1    5.430     5.412     5.400       0.048         No     Regular  Decreasing     No      Yes  
#>    NU      2    5.286     5.235     5.200       0.023         No     Regular  Decreasing    Yes      Yes  
#>    NU      3    5.412     5.375     5.400       0.043         No     Regular  Decreasing    Yes      Yes  
#>    TK      1    5.306     5.267     5.300       0.067         No     Regular  Decreasing     No      Yes  
#>    TK      2    5.185     5.154     5.200       0.046         No     Regular  Decreasing    Yes      Yes  
#>    TK      3    5.483     5.400     5.550       0.082         No    Irregular Decreasing    Yes      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✖ Possible missing dilution or irregular dilution series detected.
#> ✔ All count trends appear to decrease as x increases. 
#>   `autosort == TRUE`: decreasing response ED50 values computed using 1-y/n.
#> ✖ Some count trends are not monotonic along x.
#> ✖ Some count trends do not bracket ED50. 

## Monotonically increasing data

# The three calls are equivalent.
dead  <- c(0, 3, 5, 8, 10, 10)
total <- rep(10, 6)
dil   <- 1:6

## Use numeric vectors in the formula
DragBehr(dead + total ~ dil)
#> skrmdb :: Autosorting dilution sequences.
#> ED50 by the Dragstedt-Behrens method.
#> 
#>   ed  Duplicate Dilutions  Response  Monotonic Bracket
#> 2.907     No     Regular  Increasing    Yes      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to increase as x increases.
#> ✔ All count trends are monotonic along x.
#> ✔ All count trends bracket ED50.
#>  
ReedMuench(dead + total ~ dil)
#> skrmdb :: Autosorting dilution sequences.
#> ED50 by the Reed-Muench method.
#> 
#>   ed  Duplicate Dilutions  Response  Monotonic Bracket
#> 2.917     No     Regular  Increasing    Yes      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to increase as x increases.
#> ✔ All count trends are monotonic along x.
#> ✔ All count trends bracket ED50.
#>  
SpearKarb(dead + total ~ dil)
#> skrmdb :: Autosorting dilution sequences.
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
skrmdb.all(dead + total ~ dil)
#> skrmdb :: Autosorting dilution sequences.
#> ED50 by the all skrmdb methods.
#> 
#> DragBehr ReedMuench SpearKarb SpearKarb.var Duplicate Dilutions  Response  Monotonic Bracket
#>   2.907     2.917     2.900       0.069         No     Regular  Increasing    Yes      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to increase as x increases.
#> ✔ All count trends are monotonic along x.
#> ✔ All count trends bracket ED50.
#>  

data  <- data.frame(y = dead,
                    n = total,
                    x = dil)

## Use data plus the formula
DragBehr(data, y + n ~ x)
#> skrmdb :: Autosorting dilution sequences.
#> ED50 by the Dragstedt-Behrens method.
#> 
#>   ed  Duplicate Dilutions  Response  Monotonic Bracket
#> 2.907     No     Regular  Increasing    Yes      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to increase as x increases.
#> ✔ All count trends are monotonic along x.
#> ✔ All count trends bracket ED50.
#>  
ReedMuench(data, y + n ~ x)
#> skrmdb :: Autosorting dilution sequences.
#> ED50 by the Reed-Muench method.
#> 
#>   ed  Duplicate Dilutions  Response  Monotonic Bracket
#> 2.917     No     Regular  Increasing    Yes      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to increase as x increases.
#> ✔ All count trends are monotonic along x.
#> ✔ All count trends bracket ED50.
#>  
SpearKarb(data, y + n ~ x)
#> skrmdb :: Autosorting dilution sequences.
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
skrmdb.all(data, y + n ~ x)
#> skrmdb :: Autosorting dilution sequences.
#> ED50 by the all skrmdb methods.
#> 
#> DragBehr ReedMuench SpearKarb SpearKarb.var Duplicate Dilutions  Response  Monotonic Bracket
#>   2.907     2.917     2.900       0.069         No     Regular  Increasing    Yes      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to increase as x increases.
#> ✔ All count trends are monotonic along x.
#> ✔ All count trends bracket ED50.
#>  


## Using y, n, x is deprecated.
DragBehr(y = dead, n = total, x = dil) |> suppressWarnings()
#> skrmdb :: Autosorting dilution sequences.
#> ED50 by the Dragstedt-Behrens method.
#> 
#>   ed  Duplicate Dilutions  Response  Monotonic Bracket
#> 2.907     No     Regular  Increasing    Yes      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to increase as x increases.
#> ✔ All count trends are monotonic along x.
#> ✔ All count trends bracket ED50.
#>  
ReedMuench(y = dead, n = total, x = dil) |> suppressWarnings()
#> skrmdb :: Autosorting dilution sequences.
#> ED50 by the Reed-Muench method.
#> 
#>   ed  Duplicate Dilutions  Response  Monotonic Bracket
#> 2.917     No     Regular  Increasing    Yes      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to increase as x increases.
#> ✔ All count trends are monotonic along x.
#> ✔ All count trends bracket ED50.
#>  
SpearKarb(y = dead, n = total, x = dil) |> suppressWarnings()
#> skrmdb :: Autosorting dilution sequences.
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

# The function automatically automatically reorders the data as needed to
# meet the ordering assumption.
dead  <- rev(dead)
total <- rev(total)
dil   <- rev(dil)
SpearKarb(dead + total ~ dil)
#> skrmdb :: Autosorting dilution sequences.
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

## Unordered data
# Observe that the data is not monotonic after being sorted by dil.
dead  <- c(10, 8, 5, 3, 0)
total <- rep(10, 5)
dil   <- c(1, 3, 2, 4, 5)
SpearKarb(dead + total ~ dil)
#> skrmdb :: y is not monotonic. ED50 may be unreliable.
#> ED50 by the Spearman-Karber method.
#> 
#>   ed   var  Duplicate Dilutions  Response  Monotonic Bracket
#> 3.100 0.069     No     Regular  Decreasing     No      Yes   
#> 
#> ✔ No duplicate dilutions detected.
#> ✔ All dilution series appear to be regular.
#> ✔ All count trends appear to decrease as x increases. 
#>   `autosort == TRUE`: decreasing response ED50 values computed using 1-y/n.
#> ✖ Some count trends are not monotonic along x.
#> ✔ All count trends bracket ED50.
#>  
```

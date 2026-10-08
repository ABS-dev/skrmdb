# Changelog

## Version 5.05

- Added a vignette to explain how Dragstedt-Behrens and Reed-Muench are
  computed by hand.

## Version 5.0.4

- Revised documentation and vignette. In particular, the basic
  assumptions of
  [`SpearKarb()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
  [`DragBehr()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
  and
  [`ReedMuench()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md)
  are now explained in both the vignette and function documentation.
  Added explanations of feedback messages provided by the functions.
- Issue [\#4](https://github.com/ABS-dev/skrmdb/issues4): Clarified
  obscure error messages that occur when one of the count variables is
  `NA` or when the number positive exceeds the total number.
- Issue [\#6](https://github.com/ABS-dev/skrmdb/issues6): Changed
  `"even"` dilution scheme to `"Regular"` dilution scheme and clarified
  the message to indicate that a potential missing dilution was
  detected. Updated code to reduce false positives.
- Issue [\#7](https://github.com/ABS-dev/skrmdb/issues7): Report when
  there is no change in the response, so constant responses are no
  longer incorrectly identified as either `"increasing"` or
  `"decreasing"`.
- `news(package = "skrmdb")` works when package it not loaded.

## Version 5.0.3

- Issue [\#5](https://github.com/ABS-dev/skrmdb/issues5): The `results`
  element of an `skrmdb` object is no longer returned invisibly the
  first time it is accessed.

## Version 5.0.2

- Commented out a debugging print statement that was accidentally left
  in.

## Version 5.0.1

- Updated GitLab URL.
- Renamed default branch from “master” to “main”.

## Version 5.0.0

- Functions for computing ED50 now accept formulas of the form
  `y + n ~ x` or `y + n ~ x | w1 + ... + wk`, allowing ED50 to be
  computed for multiple groups in one call.
- The [`print()`](https://rdrr.io/r/base/print.html) method now prints a
  results table with feedback on how well the data meet the assumptions
  used by the methods to compute ED50.
- [`getdata()`](https://abs-dev.github.io/skrmdb/reference/skrmdb-class.md)
  and
  [`getvar()`](https://abs-dev.github.io/skrmdb/reference/skrmdb-class.md)
  have been deprecated in favor of
  [`getData()`](https://abs-dev.github.io/skrmdb/reference/skrmdb-class.md)
  and
  [`getVar()`](https://abs-dev.github.io/skrmdb/reference/skrmdb-class.md)
  to provide consistent camelCase function naming.
- Extended vignette.

## Version 4.5.0

- Added online documentation.
- Deprecated
  [`getED()`](https://abs-dev.github.io/skrmdb/reference/skrmdb-class.md)
  and replaced it with
  [`getED50()`](https://abs-dev.github.io/skrmdb/reference/skrmdb-class.md).
- Added vignette.

## Version 4.4.0

- Rewrote functions. Functions no longer require `data` to be sorted.
- Added warnings for possible data problems.
- Deprecated use of the `y`, `n`, and `x` parameters.
- Expanded function definitions used to set what is going on.

## Version 4.2.4

- [`SpearKarb()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
  [`ReedMuench()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
  and
  [`DragBehr()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md)
  now require input data to be sorted by `x`, either increasing or
  decreasing. No estimate is calculated if this condition is not met.
- [`SpearKarb()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
  [`ReedMuench()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
  and
  [`DragBehr()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md)
  now check whether the input `y` variable is nonmonotone and display a
  warning. The estimate is calculated in the original order regardless
  of direction.
- For monotone `y` variables,
  [`SpearKarb()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
  [`ReedMuench()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
  and
  [`DragBehr()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md)
  now check whether the input is monotone increasing or monotone
  decreasing and display a message. The estimate is calculated in the
  original order regardless of direction.
- Added examples of unordered `x` variables, `x` variable direction, `y`
  variable direction, and nonmonotone `y` variables to the
  [`SpearKarb()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
  [`ReedMuench()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md),
  and
  [`DragBehr()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md)
  help pages.

## Version 4.2.3

- Added checks to
  [`SpearKarb()`](https://abs-dev.github.io/skrmdb/reference/skrmdb.md)
  for monotone increasing input data.

# Class definition for skrmdb object

The skrmdb object holds output from functions in the skrmdb package. A
valid skrmdb object is a list whicn contains the followling components:

## Arguments

- eval:

  Evaluation method. One of "ReedMuench", "SpearKarb", or "DragBehr".
  character string.

- ed:

  Median effective dose by eval method. numeric.

- var:

  Variance (for Spearman-Kärber method only). numeric.

- data:

  The data used to compute the effective median dose. data.frame,
  numeric.

- message:

  Messages and warnings generated durning the calculation. character
  string.

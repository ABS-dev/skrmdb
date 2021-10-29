.checkmatrix <- function(A, autoarrange, warn.me) {
  A = as.data.table(A)
  setnames(A, c("y", "n", "x"))
  # put in some checking here for what if y, n, or x are not integers
  if (A[, any(c(y,n,x) != as.integer(c(y, n, x)))]) {
    stop("skrmdb :: y, n, and x must be integers")
  }
  # Then scale appropriately
  N = A[, prod(unique(n))]
  A[, y0 := y]
  A[, y := y * N / n]
  A[, n0 := n]
  A[, n := N]
  setorder(A, x)
  warning.monotone = warning.uneven = 
    warning.bracket = warning.reverse = FALSE
  if (autoarrange && A[, .N] * A[, sum(y * x)] < A[, sum(x) * sum(y)]) {
    setorder(A, -x)
    if (warn.me) message("skrmdb :: y appears to be decreasing. Reversing order of data.")
    warning.reverse = TRUE
  }
  if (length(unique(diff(zapsmall(A$x)))) > 1) {
    if (warn.me) message("skrmdb :: Uneven dilution scheme.")
    warning.uneven = TRUE
  }
  if (A[, !all(y == cummax(y))] && A[, !all(y == cummin(y))]) {
    if (warn.me) message("skrmdb :: y is not monotonic. ED50 may be unreliable.")
    warning.monotone = TRUE
  }
  if (2 * A[, min(y)] >= N || 2 * A[, max(y)] <= N) {
    if (warn.me) message("skrmdb :: Dilutions fail to bracket the midpoint. ED50 is unreliable.")
    warning.bracket = TRUE
  }
  attr(A, "warning.reverse")  = warning.reverse
  attr(A, "warning.monotone") = warning.monotone
  attr(A, "warning.uneven")   = warning.uneven
  attr(A, "warning.bracket")  = warning.bracket
  return(A)
}

.checkdata <- function(formula, data, autoarrange, wanr) {
  if (is.null(formula)) {
    return(NULL)
  }
  vars <- attr(terms(formula(f)), which = "variables")
  lhs <- as.character(vars[[2]]) 
  if (!(lhs[1] == "+" || lhs[1] == "cbind") || length(lhs) != 3) {
    return(NULL)
  }
  A <- as.matrix(model.frame(formula = Formula(formula), data = data))
  if (ncol(A) != 3) {
    return(NULL)
  }
  .checkmatrix(A, autoarrange, warn.me)
}

.checkvars <- function(y, n, x, autoarrange, warn.me) {
  # message("Depreciated: Use y + n ~ x.")
  if (missing(y) | missing(n) | missing(x)) {
    return(NULL)
  } else if ((length(y) != length(x)) | (length(y) != length(n))) {
    stop("skrmdb :: variables x, n, and y must be the same length")
  } 
  .checkmatrix(data.table(y = y, n = n, x = x), autoarrange, warn.me)
}
#' @importFrom data.table setnames setorder as.data.table := .N
.checkmatrix <- function(A, autosort = TRUE, warn.me = FALSE) {
  y <- n <- x <- y0 <- n0 <- NULL 
  A <- as.data.table(A)
  setnames(A, c("y", "n", "x"))
  # put in some checking here for what if y, n, or x are not integers
  if (A[, any(c(y,n) != as.integer(c(y, n)))]) {
    stop("skrmdb :: y and n must be integers", call. = FALSE)
  }
  # Then scale appropriately
  N <- A[, prod(unique(n))]
  A[, y0 := y]
  A[, y := y * N / n]
  A[, n0 := n]
  A[, n := N]
  warning.monotone <- warning.uneven <-
    warning.bracket <- warning.increasing <- FALSE

  S = A[, .N * sum(x * y) - sum(x) * sum(y)]
  warning.increasing <- S >= 0  # All algorithms make assumption that p's should be increasing.
  if (autosort) {
    if (S >= 0) {
      setorder(A, x)
    } else {
      setorder(A, -x)
    }
  }
  if (length(unique(diff(zapsmall(A$x)))) > 1) {
    if (warn.me) message("skrmdb :: Uneven dilution scheme.")
    warning.uneven <- TRUE
  }
  if (A[, !all(y == cummax(y))] && A[, !all(y == cummin(y))]) {
    if (warn.me) message("skrmdb :: y is not monotonic. ED50 may be unreliable.")
    warning.monotone <- TRUE
  }
  if (2 * A[, min(y)] >= N || 2 * A[, max(y)] <= N) {
    if (warn.me) message("skrmdb :: Dilutions fail to bracket the midpoint. ED50 is unreliable.")
    warning.bracket <- TRUE
  }
  attr(A, "warning.monotone")   <- warning.monotone
  attr(A, "warning.uneven")     <- warning.uneven
  attr(A, "warning.bracket")    <- warning.bracket
  attr(A, "warning.increasing") <- warning.increasing
  return(A)
}

#' @importFrom stats model.frame formula terms
#' @importFrom Formula Formula
.checkdata <- function(formula, data, autosort = TRUE, warn.me = FALSE) {
  if (is.null(formula)) {
    return(NULL)
  }
  vars <- attr(terms(formula(formula)), which = "variables")
  lhs <- as.character(vars[[2]]) 
  if (!(lhs[1] == "+" || lhs[1] == "cbind") || length(lhs) != 3) {
    stop("skrmdb :: formula must be of form y + n ~ x.", call. = FALSE)
  }
  A <- as.matrix(model.frame(formula = Formula(formula), data = data))
  if (ncol(A) != 3) {
    stop("skrmdb :: formula must be for form y + n ~ x where y, n, and x are all different.", call. = FALSE)
  }
  .checkmatrix(A, autosort, warn.me)
}

#' @importFrom data.table data.table
.checkvars <- function(y, n, x, autosort = TRUE, warn.me = FALSE) {
  # message("Depreciated: Use y + n ~ x.")
  if (missing(y) | missing(n) | missing(x)) {
    return(NULL)
  } else if ((length(y) != length(x)) | (length(y) != length(n))) {
    stop("skrmdb :: variables x, n, and y must be the same length", call. = FALSE)
  } 
  .checkmatrix(data.table(y = y, n = n, x = x), autosort, warn.me)
}

.checkall <- function(formula, data, y, n, x, autosort = TRUE, warn.me = FALSE) {
  A <- .checkdata(formula, data, autosort, warn.me)
  if (is.null(A)) {
    A <- .checkvars(y, n, x, autosort, warn.me)
  }
  return(A)
}

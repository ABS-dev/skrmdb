#' \code{.checkmatrix}
#' 
#' Scales \code{y} and \code{n} so that n is constant.
#' 
#' If \code{autosort == TRUE}, sorts \code{A} according to \code{x} or \code{-x} so that \code{y}
#' is increasing with the index.
#' 
#' Checks the matrix \code{A} for common issues which would make the computation of ED50 less 
#' reliable.
#' 1. \code{y} is not increasing with the index
#' 2. Uneven dilution scheme
#' 3. Duplicate dilutions
#' 4. Not monotonic
#' 5. y/n does not bracketed the midpoint
#' 
#' @param A a data.frame.  Assumes that the first column is the number responding at each level,
#'   the second column is the number tested at each level and the third column is the dilution
#'   at each level.
#' @param autosort Logical.  If \code{TRUE} sorts \code{A} according to \code{dilution} or 
#'   \code{-dilution} so that \code{y} is increasing with the index.
#' @param warn.me Logical.  If \code{TRUE} will display the warning messages possibilities 
#'   2-4 above.
#'   
#' @return The sorted and scaled data.frame \code{A} with attribute flags indicating the status
#'   of each warning.
#'  
#' @importFrom data.table setnames setorder as.data.table := .N
#' 
#' @noRd
.checkmatrix <- function(A, autosort = TRUE, warn.me = FALSE) {
  y_inc <- y_dec <- y <- n <- x <- NULL
  # y <- n <- x <- y0 <- n0 <- NULL 
  warning.monotonic <- warning.uneven <- warning.bracket <- 
    warning.increasing <- warning.duplicate <- FALSE
  A <- as.data.table(A)
  setnames(A, c("y", "n", "x"))
  # put in some checking here for what if y, n, or x are not integers
  if (A[, any(c(y,n) != as.integer(c(y, n)))]) {
    stop("skrmdb :: y and n must be integers", call. = FALSE)
  }
  # Look for duplicate dilutions and consolidate
  n1 <- A[, .N]
  A <- A[, .(y = sum(y), n = sum(n)), x]
  if (A[, .N < n1]) {
    if (warn.me) message("skrmdb :: combining results from duplicate dilutions")
    warning.duplicate <- TRUE
  }
  # Sort by dilution
  setorder(A, x)
  # Then scale appropriately since the functions assume that n is constant
  N <- A[, prod(unique(n))]
  Y1 <- A[, y * N / n]
  Y2 <- N - Y1
  warning.increasing <- A[, .N * sum(x * Y1) - sum(x) * sum(Y1)] >= 0  
  if (autosort & !warning.increasing) {
    A[, y_inc := Y2]
    A[, y_dec := Y1]
  } else {
    A[, y_inc := Y1]
    A[, y_dec := Y2]
  }
  if (length(unique(diff(zapsmall(A$x)))) > 1) {
    if (warn.me) message("skrmdb :: Uneven dilution scheme.")
    warning.uneven <- TRUE
  }
  if (A[, !all(y_inc == cummax(y_inc))] && A[, !all(y_inc == cummin(y_inc))]) {
    if (warn.me) message("skrmdb :: y is not monotonic. ED50 may be unreliable.")
    warning.monotonic <- TRUE
  }
  if (2 * A[, min(y_inc)] >= N || 2 * A[, max(y_inc)] <= N) {
    if (warn.me) message("skrmdb :: Dilutions fail to bracket the midpoint. ED50 is unreliable.")
    warning.bracket <- TRUE
  }
  attr(A, "warning.monotonic")  <- warning.monotonic
  attr(A, "warning.uneven")     <- warning.uneven
  attr(A, "warning.bracket")    <- warning.bracket
  attr(A, "warning.increasing") <- warning.increasing
  attr(A, "warning.duplicate")  <- warning.duplicate
  return(A)
}

#' \code{.checkdata}
#' 
#' Checks that formula is of the form \code{y + n ~ x} and that all variables are in the data.frame
#' \code{A}.  
#' 
#' @param formula a formula
#' @param data a data.table
#' @param autosort Logical.  If TRUE indicates that the code should automatically sort \code{A} so 
#'   that the number responding is increasing with the index.
#' @param warn.me Logical.  If TRUE indicates that data warnings will be displayed during 
#'   processing. 
#' 
#' @return the result from \code{.checkmatrix}.
#' 
#' @importFrom stats model.frame formula terms
#' @importFrom Formula Formula
#' 
#' @noRd
.checkdata <- function(formula, data, autosort = TRUE, warn.me = FALSE) {
  if (missing(formula)) {
    return(NULL)
  }
  if (missing(data)) {
    data <- NULL
  }
  vars <- attr(terms(formula(formula)), which = "variables")
  lhs <- as.character(vars[[2]]) 
  if (!(lhs[1] == "+" || lhs[1] == "cbind") || length(lhs) != 3) {
    stop("skrmdb :: formula must be of form y + n ~ x.", call. = FALSE)
  }
  A <- as.matrix(model.frame(formula = Formula(formula), data = data))
  if (ncol(A) != 3) {
    stop("skrmdb :: formula must be of the form y + n ~ x where y, n, and x are all different.", 
         call. = FALSE)
  }
  .checkmatrix(A, autosort, warn.me)
}

#' \code{.checkvars}
#' 
#' Depreciated.  Checks to see if \code{y}, \code{n}, \code{x} were used instead of passing the 
#' function a formula and data.
#' 
#' @param y Integer vector.  Number of subjects responding at each level.  
#'   Assumed to be increasing.
#' @param n Integer vector.  Number of subjects tested at each level.  
#'   Assumed to be constant.
#' @param x Integer vector.  The dilution at each level.
#' @param autosort Logical.  If TRUE indicates that the code should automatically sort \code{A} so 
#'   that the number responding is increasing with the index.
#' @param warn.me Logical.  If TRUE indicates that data warnings will be displayed during 
#'   processing. 
#'
#' @return the result from \code{.checkmatrix}.
#'
#' @importFrom data.table data.table
#' 
#' @noRd
.checkvars <- function(y, n, x, autosort = TRUE, warn.me = FALSE) {
  if (missing(y) | missing(n) | missing(x)) {
    return(NULL)
  } else if ((length(y) != length(x)) | (length(y) != length(n))) {
    stop("skrmdb :: variables x, n, and y must be the same length", call. = FALSE)
  } 
  .checkmatrix(data.table(y = y, n = n, x = x), autosort, warn.me)
}

#' \code{.checkall}
#' 
#' Checks to see how the ED50 funcions were called: either via a formula and data or by passing 
#' \code{y}, \code{n}, and \code{x} directly.
#' 
#' @param formula a formula of the form \code{y + n ~ x} or \code{cbind(y, n) ~ x}
#' @param data a data frame
#' @param y an integer vector corresponding to the number responding at each log dilution or dose.
#' @param n an integer vector corresponding to the group size at each log dilution or dose.
#' @param x a vector corresponding to the log dilution or dose for each group.
#' @param autosort Default \code{TRUE}.  If \code{TRUE} will sort the data according to either
#'   \code{sort(x)} or \code{sort(-x)} so that \code{y / n} appears to be increasing with the
#'   index.  This is how the three methods assume the data to be ordered.
#' @param warn.me if TRUE, warnings and messages related to the processing of the data will be
#'   displayed.
#' 
#' @return a data.table \code{A} which contains all the data to be processed.  The data has been
#' prepared and potentially sorted for use by the \code{.checkmatrix}.
#' 
#' @noRd
.checkall <- function(formula, data, y, n, x, autosort = TRUE, warn.me = FALSE) {
  A <- .checkdata(formula, data, autosort, warn.me)
  if (is.null(A)) {
    warning("skrmdb :: Calling this function with the parameters y, n, x is depreciated.",
            call. = FALSE)
    A <- .checkvars(y, n, x, autosort, warn.me)
  }
  return(A)
}

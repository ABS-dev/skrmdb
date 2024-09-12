#' Reed-Muench algorithm
#'
#' Implements the Reed-Muench Algorithm as found in Miller, 1973
#'
#' @param y Integer vector.  Number of subjects responding at each level.
#'   Assumed to be increasing.
#' @param n Integer vector.  Number of subjects tested at each level. Assumed to
#'   be constant.
#' @param x Integer vector.  The dilution at each level.
#' @param show logical.  If TRUE shows the intermediary statistics
#'
#' @return Numeric.  ED50 as computed by Reed-Muench.
#'
#' @noRd
.ReedMuench <- function(y_inc, y_dec, x, show = FALSE) {
  n <- y_inc[1] + y_dec[1]
  a <- cumsum(y_inc)
  b <- rev(cumsum(rev(y_dec)))
  diff <- a - b
  if (any(diff == 0)) {
    ed <- mean(x[which(diff == 0)])
  } else {
    i <- max(c(-Inf, which(diff < 0)))
    ed <- x[i] +
      ((x[i + 1] - x[i]) * (b[i] - a[i])) / (n - y_inc[i] + y_inc[i + 1])
  }

  if (show) print(data.frame(x, y_inc, y_dec, a, b, P = round(a / (a + b), 2)))
  return(ed)
}

#' @rdname skrmdb
#'
#' @export
ReedMuench <- function(formula, data, y, n, x,
                       autosort = TRUE, warn.me = TRUE, show = FALSE) {
  A <- .checkall(formula, data, y, n, x, autosort, warn.me)
  return(new_skrmdb("Reed-Muench", A, .ReedMuench(A$y_inc, A$y_dec, A$x, show)))
}

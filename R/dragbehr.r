#' Dragstedt-Behrens algorithm
#'
#' Implements the Dragstedt-Behrens Algorithm as found in Miller, 1973
#'
#' @param y Integer vector.  Number of subjects responding at each level.
#'   Assumed to be increasing.
#' @param n Integer vector.  Number of subjects tested at each level.
#'   Assumed to be constant.
#' @param x Integer vector.  The dilution at each level.
#' @param show logical.  If TRUE shows the intermediary statistics
#'
#' @returns Numeric.  ED50 as computed by Dragstedt-Behrens.
#'
#' @noRd
.DragBehr <- function(y_inc, y_dec, x, show = FALSE) {
  a <- cumsum(y_inc)
  b <- rev(cumsum(rev(y_dec)))
  P <- a / (a + b)
  diff <- a - b
  if (any(diff == 0)) {
    ed <- mean(x[which(diff == 0)])
  } else {
    i <- max(c(-Inf, which(diff < 0)))
    ed <- x[i] + (((x[i + 1] - x[i]) * (1 / 2 - P[i])) / (P[i + 1] - P[i]))
  }

  if (show) print(data.frame(x, y_inc, y_dec, a, b, P = round(P, 2)))
  return(ed)
}

#' @rdname skrmdb
#'
#' @export
DragBehr <- function(formula, data, y, n, x,
                     autosort = TRUE, warn.me = TRUE, show = FALSE) {
  A <- .checkall(formula, data, y, n, x, autosort, warn.me)
  return(new_skrmdb("Dragstedt-Behrens", A,
                    .DragBehr(A$y_inc, A$y_dec, A$x, show)))
}

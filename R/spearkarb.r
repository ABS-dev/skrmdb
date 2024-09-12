#' Spearman-Kärber algorithm
#' 
#' Implements the Spearman-Kärber Algorithm as found in Miller, 1973
#' 
#' @param y Integer vector.  Number of subjects responding at each level.  
#'   Assumed to be increasing.
#' @param n Integer vector.  Number of subjects tested at each level.  
#'   Assumed to be constant.
#' @param x Integer vector.  The dilution at each level.
#' @param n0 The original number of subjects tested at each level before scaling by 
#'   \code{.checkmatrix}.
#' @param show logical.  If TRUE shows the intermediary statistics
#' 
#' @return Numeric.  ED50 as computed by Spearman-Kärber.
#' 
#' @noRd
.SpearKarb <- function(y_inc, y_dec, x, n, show = FALSE) {
  K <- length(x)
  p  <- y_inc / (y_inc + y_dec)
  dp <- c(p, 1) - c(0, p)
  dx <- c(2 * x[1] - x[2], x) + c(x, 2 * x[K] - x[K - 1])
  ed <- 0.5 * sum(dp * dx)
  
  dx <- c(2 * (x[2] - x[1]), x[3:K] - x[1:(K - 2)], 2 * (x[K] - x[K - 1]))
  var <- sum((dx^2 * p * (1 - p)) / (4 * (n - 1)))
  
  if (show) print(data.frame(x, y_inc, y_dec, n, p = round(p, 2)))
  return(c(ed, var))
}

#' @rdname skrmdb2
#' 
#' @export
SpearKarb <- function(formula, data, y, n, x, 
                     autosort = TRUE, warn.me = TRUE, show = FALSE) {
  A <- .checkall(formula, data, y, n, x, autosort, warn.me)
  res = .SpearKarb(A$y_inc, A$y_dec, A$x, A$n, show)
  return(new_skrmdb("Spearman-K\344rber", A, res[1], res[2]))
}

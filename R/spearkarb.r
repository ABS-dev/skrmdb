#' @rdname skrmdb
#' 
#' @export
SpearKarb <- function(formula = NULL, data = NULL, y, n, x, 
                      autoarrange = TRUE, warn.me = TRUE, show = FALSE) {
  A <- .checkdata(formula, data, autoarrange, warn.me)
  if (is.null(A)) {
    A <- .checkvars(y, n, x, autoarrange, warn.me)
  }
  y <- A$y
  n <- A$n
  x <- A$x
  K <- length(x)
  
  p  <- y / n
  dp <- diff(c(0, p, 1))
  initial <- 2 * x[1] - x[2]
  final   <- 2 * x[K] - x[K - 1]
  ed <- 0.5 * sum(dp * (c(x, final) + c(initial, x)))
  
  d <- diff(c(initial, x))
  var <- sum((d^2 * p * (1 - p)) / (A$n0 - 1))
  
  if (show) print(data.frame(x, y, n, y0 = A$y0, n0 = A$n0, p = round(p, 2)))
  return(new_skrmdb("Spearman-K\344rber", A, ed, var))
}

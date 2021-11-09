.SpearKarb <- function(y, n, x, n0, show = FALSE) {
  K <- length(x)
  p  <- y / n
  dp <- c(p, 1) - c(0, p)
  dx <- c(2 * x[1] - x[2], x) + c(x, 2 * x[K] - x[K - 1])
  ed <- 0.5 * sum(dp * dx)
  
  dx <- c(2 * (x[2] - x[1]), x[3:K] - x[1:(K - 2)], 2 * (x[K] - x[K - 1]))
  var <- sum((dx^2 * p * (1 - p)) / (4 * (n0 - 1)))
  
  if (show) print(data.frame(x, y, n, n0, p = round(p, 2)))
  return(c(ed, var))
}

#' @rdname skrmdb
#' 
#' @export
SpearKarb <- function(formula = NULL, data = NULL, y, n, x, 
                     autosort = TRUE, warn.me = TRUE, show = FALSE) {
  A <- .checkall(formula, data, y, n, x, autosort, warn.me)
  res = .SpearKarb(A$y, A$n, A$x, A$n0, show)
  return(new_skrmdb("Spearman-K\344rber", A, res[1], res[2]))
}

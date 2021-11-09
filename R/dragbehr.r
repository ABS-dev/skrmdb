.DragBehr <- function(y, n, x, show = FALSE) {
  a <- cumsum(y)
  b <- rev(cumsum(rev(n - y)))
  P <- a / (a + b)
  diff <- a - b
  if (any(diff == 0)) {
    ed <- mean(x[which(diff == 0)])
  } else {
    i <- max(c(-Inf, which(diff < 0)))
    ed <- x[i] + (((x[i + 1] - x[i]) * (1 / 2 - P[i])) / (P[i + 1] - P[i]))
  }
  
  if (show) print(data.frame(x, y, n, a, b, P = round(P, 2)))
  return(ed)
}

#' @rdname skrmdb
#' 
#' @export
DragBehr <- function(formula = NULL, data = NULL, y, n, x, 
                     autosort = TRUE, warn.me = TRUE, show = FALSE) {
  A <- .checkall(formula, data, y, n, x, autosort, warn.me)
  return(new_skrmdb("Dragstedt-Behrens", A, .DragBehr(A$y, A$n, A$x, show)))
}


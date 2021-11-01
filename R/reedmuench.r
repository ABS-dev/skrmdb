#' @rdname skrmdb
#'
#' @export
ReedMuench <- function(formula = NULL, data = NULL, y, n, x, 
                       autoarrange = TRUE, warn.me = TRUE, show = FALSE) {
  A <- .checkdata(formula, data, autoarrange, warn.me)
  if (is.null(A)) {
    A <- .checkvars(y, n, x, autoarrange, warn.me)
  }
  y <- A$y
  n <- A$n
  x <- A$x
  
  a <- cumsum(y)
  b <- rev(cumsum(rev(n - y)))
  diff <- a - b
  if (any(diff == 0)) {
    ed <- mean(x[which(diff == 0)])
  } else {
    i <- max(which(diff < 0))
    ed <- x[i] + ((x[i + 1] - x[i]) * (b[i] - a[i])) / (n[i] - y[i] + y[i + 1])
  }    
  
  if (show) print(data.frame(x, y, n, y0 = A$y0, n0 = A$n0, a, b, P = round(a / (a + b), 2)))
  return(new_skrmdb("Reed-Muench", A, ed))
}

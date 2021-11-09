.ReedMuench <- function(y, n, x, show = FALSE) {
  a <- cumsum(y)
  b <- rev(cumsum(rev(n - y)))
  diff <- a - b
  if (any(diff == 0)) {
    ed <- mean(x[which(diff == 0)])
  } else {
    i <- max(c(-Inf, which(diff < 0)))
    ed <- x[i] + ((x[i + 1] - x[i]) * (b[i] - a[i])) / (n[i] - y[i] + y[i + 1])
  }    
  
  if (show) print(data.frame(x, y, n, a, b, P = round(a / (a + b), 2)))
  return(ed)
}

#' @rdname skrmdb
#'
#' @export
ReedMuench <- function(formula = NULL, data = NULL, y, n, x, 
                     autosort = TRUE, warn.me = TRUE, show = FALSE) {
  A <- .checkall(formula, data, y, n, x, autosort, warn.me)
  return(new_skrmdb("Reed-Muench", A, .ReedMuench(A$y, A$n, A$x, show)))
}

#' @title Reed-Muench algorithm
#'
#' @description Implements the Reed-Muench Algorithm as found in Miller, 1973.
#'
#' @param dt A `data.table` with the appropriate columns.
#' @param show logical.  If TRUE shows the intermediary statistics
#'
#' @returns Numeric.  ED50 as computed by Reed-Muench.
#'
#' @noRd
.ReedMuench <- function(dt, show = FALSE) {
  y_inc <- y_dec <- x <- NULL
  x <- dt[, x]
  n <- dt[1, y_inc + y_dec]
  a <- dt[, cumsum(y_inc)]
  b <- dt[, rev(cumsum(rev(y_dec)))]
  diff <- zapsmall(a - b)
  if (any(diff == 0)) {
    ed <- mean(x[which(diff == 0)])
  } else {
    i <- max(c(-Inf, which(diff < 0)))
    y_inc <- dt[, y_inc]
    ed <- x[i] +
      ((x[i + 1] - x[i]) * (b[i] - a[i])) / (n - y_inc[i] + y_inc[i + 1])
  }
  if (show) print(data.frame(x,
                             y_inc = dt$y_inc,
                             y_dec = dt$y_dec,
                             a,
                             b,
                             P = round(a / (a + b), 2)))
  list(ed = ed)
}

#' @rdname skrmdb
#'
#' @importFrom data.table .SD
#' @importFrom lifecycle deprecated deprecate_warn is_present
#' @export
ReedMuench <- function(
    data,
    formula,
    autosort = TRUE,
    warn.me  = TRUE,
    show     = FALSE,
    y        = deprecated(),
    n        = deprecated(),
    x        = deprecated()) {
  if (is_present(y) || is_present(n) || is_present(x)) {
    deprecate_warn(
      when     = "5.0.0",
      what     = I("The `y`, `n`, and `x` arguments of `ReedMuench()`"),
      details  = "Use the `data` and `formula` arguments instead.",
      always   = TRUE)
    lst <- .parse_data(y        = y,
                       n        = n,
                       x        = x,
                       autosort = autosort,
                       warn.me  = warn.me)
  } else {
    lst <- .parse_data(data     = data,
                       formula  = formula,
                       autosort = autosort,
                       warn.me  = warn.me)
  }
  groups       <- c(lst$levs, lst$msg)
  lst$results  <- lst$data[, .ReedMuench(.SD, show), groups]
  lst$autosort <- autosort
  new_skrmdb("Reed-Muench", lst)
}

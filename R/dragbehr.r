#' @title Dragstedt-Behrens algorithm
#'
#' @description Implements the Dragstedt-Behrens Algorithm as found in Miller,
#'   1973.
#'
#' @param dt A `data.table` with the appropriate columns.
#' @param show logical.  If TRUE shows the intermediary statistics
#'
#' @returns Numeric.  ED50 as computed by Dragstedt-Behrens.
#'
#' @noRd
.DragBehr <- function(dt, show = FALSE) {
  y_inc <- y_dec <- x <- NULL
  x    <- dt[, x]
  a    <- dt[, cumsum(y_inc)]
  b    <- dt[, rev(cumsum(rev(y_dec)))]
  p    <- a / (a + b)
  diff <- zapsmall(a - b)
  if (any(diff == 0)) {
    ed <- mean(x[which(diff == 0)])
  } else {
    i <- max(c(-Inf, which(diff < 0)))
    ed <- x[i] + (((x[i + 1] - x[i]) * (1 / 2 - p[i])) / (p[i + 1] - p[i]))
  }
  if (show) print(data.frame(x,
                             y_inc = dt$y_inc,
                             y_dec = dt$y_dec,
                             a,
                             b,
                             P = round(p, 2)))
  list(ed = ed)
}

#' @rdname skrmdb
#'
#' @importFrom data.table .SD
#' @importFrom lifecycle deprecated deprecate_warn is_present
#' @export
DragBehr <- function(
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
      what     = I("The `y`, `n`, and `x` arguments of `DragBehr()`"),
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
  lst$results  <- lst$data[, .DragBehr(.SD, show), groups]
  lst$autosort <- autosort
  new_skrmdb("Dragstedt-Behrens", lst)
}

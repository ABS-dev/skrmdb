#' @title Spearman-Kärber algorithm
#'
#' @description Implements the Spearman-Kärber Algorithm as found in Miller,
#'   1973.
#'
#' @param dt A `data.table` with the appropriate columns.
#' @param show logical.  If TRUE shows the intermediary statistics
#'
#' @returns Numeric.  ED50 as computed by Spearman-Kärber.
#'
#' @importFrom data.table .N
#' @noRd
.SpearKarb <- function(dt, show = FALSE) {
  y_dec <- y_inc <- x <- n <- NULL
  x   <- dt[, x]
  n   <- dt[, n]
  k   <- dt[, .N]
  p   <- dt[, y_inc / (y_inc + y_dec)]
  dp  <- c(p, 1) - c(0, p)
  x   <- c(2 * x[1] - x[2], x, 2 * x[k] - x[k - 1])
  dx  <- x[-(k + 2)] + x[-1]
  ed  <- 0.5 * sum(dp * dx)
  dx  <- x[-(1:2)] - x[-((k + 1):(k + 2))]
  var <- sum((dx^2 * p * (1 - p)) / (4 * (n - 1)))

  if (show) print(data.frame(x[c(-1, k + 2)],
                             y_inc = dt$y_inc,
                             y_dec = dt$y_dec,
                             n,
                             p = round(p, 2)))
  list(ed = ed, var = var)
}

#' @rdname skrmdb
#'
#' @importFrom data.table .SD
#' @importFrom lifecycle deprecated deprecate_warn is_present
#' @export
SpearKarb <- function(
    data,
    formula,
    autosort = TRUE,
    warn.me  = TRUE,
    show     = FALSE,
    y        = deprecated(),
    n        = deprecated(),
    x        = deprecated()) {
  ed <- msg_response <- NULL
  if (is_present(y) || is_present(n) || is_present(x)) {
    deprecate_warn(
      when     = "5.0.0",
      what     = I("The `y`, `n`, and `x` arguments of `SpearKarb()`"),
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
  lst$results  <- lst$data[, .SpearKarb(.SD, show), groups] |>
    _[msg_response == 0, ed := NA]
  lst$autosort <- autosort
  new_skrmdb("Spearman-Karber", lst)
}

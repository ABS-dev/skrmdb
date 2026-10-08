#' @importFrom stats setNames
#' @importFrom data.table .N
.parse_formula <- function(
    data,
    formula,
    y,
    n,
    x) {
  target <- user <- NULL
  if (missing(data) && missing(formula)) {
    if (!missing(y) && !missing(n) && !missing(x)) {
      if (length(y) != length(n) || length(y) != length(x)) {
        stop("skrmdb :: the parameters `y`, `n`, and `x` should all be of the ",
             "same length",
             call. = FALSE)
      }
      fmla <- CVB_formula$new(y + n ~ x, y + n ~ x)
    } else {
      stop("skrmdb :: The parameters `data` and `formula` should be specified",
           call. = FALSE)
    }
  } else {
    if (!missing(data) && inherits(data, "formula")) {
      tmp <- data
      if (!missing(formula)) {
        data    <- formula
      } else {
        data    <- NULL
      }
      formula <- tmp
    }
    fmla <- tryCatch(
      CVB_formula$new(formula, y + n ~ x, y + n ~ x | ., data = data),
      error = function(e) {
        stop("skrmdb :: Something went wrong.  Last error message: ", e,
             call. = FALSE)
      }
    )
  }
  fmla$check_types(
    y = "count",
    n = "count",
    x = "numeric"
  )
  tryCatch(
    fmla$check_relationships(1 <= n),
    error = function(e) {
      var <- fmla$terms[, paste(user, collapse = ", ")]
      msg <- paste("Something is wrong with the data.\n",
                   "Possible Solution: Fix or remove any row or column",
                   "for which\n",
                   var, "is <NA>.")
      stop(msg, call. = FALSE)
    })
  tryCatch(
    fmla$check_relationships(y <= n),
    error = function(e) {
      y <- fmla$terms[target == "y", user]
      n <- fmla$terms[target == "n", user]
      msg <- paste("Something is wrong with the data.\n",
                   "Solution: Check all rows where", y, ">", n)
      stop(msg, call. = FALSE)
    })

  fmla
}

.gcd <- function(a, b) {
  if (a == b || a == 0 || b == 0) return(max(a, b))
  if (a > b) {
    t <- a
    a <- b
    b <- t
  }
  .gcd(b %% a, a)
}

.lcm <- function(...) {
  list(...) |>
    unlist() |>
    unique() |>
    sort() |>
    rev() ->
    x
  if (length(x) == 1) return(x)
  t <- 1
  for (i in seq_along(x)) {
    if (i == 1) {
      t <- x[i]
    } else {
      t <- t * x[i] / .gcd(t, x[i])
    }
  }
  t
}

#' @importFrom data.table .N .SD := fifelse setcolorder setkey
#' @importFrom stringr str_detect
.parse_data <- function(
    data,
    formula,
    y,
    n,
    x,
    autosort = TRUE,
    warn.me  = FALSE) {
  target <- user <- y_inc <- y_dec <- y1 <- y2 <- N <- ._tmp__lev <-
    msg_monotonic <- msg_dilutions <- msg_bracket <-
    msg_response <- msg_duplicate <- NULL

  fmla   <- .parse_formula(data, formula, y, n, x)
  dt     <- fmla$data
  dt[, ._tmp__lev := "A"]
  terms  <- fmla$terms
  levs   <- c("._tmp__lev", terms[target == ".", user])
  levs_x <- c(levs, "x")

  # Look for duplicate dilutions and consolidate, sorting by dilution.
  dt[, msg_duplicate := .N > 1, levs_x]
  dt <- dt[, list(y = sum(y), n = sum(n)), keyby = c(levs_x, "msg_duplicate")]
  if (warn.me && dt[, any(msg_duplicate)]) {
    message("skrmdb :: Combining results from duplicate dilutions.")
  }

  dt[, msg_dilutions := length(unique(zapsmall(diff(x)))) > 1, levs]
  if (warn.me && dt[, any(msg_dilutions)]) {
    message("skrmdb :: Possible missing dilution or ",
            "irregular dilution series detected.")
  }

  # Scale appropriately since the functions assume that n is constant.
  dt[, N := .lcm(n), levs]
  dt[, y1 := y * N / n]
  dt[, y2 := N - y1]

  # This is the part that auto-sorts according to the best guess.
  dt[, msg_response := sign(zapsmall(.N * sum(x * y1) - sum(x) * sum(y1))),
     levs]

  if (autosort) {
    dt[, y_inc := fifelse(msg_response == 1, y1, y2)]
    dt[, y_dec := fifelse(msg_response == 1, y2, y1)]
  } else {
    dt[, y_inc := y1]
    dt[, y_dec := y2]
  }

  if (warn.me && dt[, any(msg_response == 1)]) {
    message("skrmdb :: Autosorting dilution sequences.")
  }

  dt[, msg_monotonic :=
       !all(y_inc == cummax(y_inc)) &&
       !all(y_inc == cummin(y_inc)), levs]
  if (warn.me && dt[, any(msg_monotonic)]) {
    message("skrmdb :: y is not monotonic. ED50 may be unreliable.")
  }

  dt[, msg_bracket := (N <= 2 * min(y_inc)) | (2 * max(y_inc) <= N), levs]
  if (warn.me && dt[, any(msg_bracket)]) {
    message("skrmdb :: Dilutions fail to bracket the midpoint. ",
            "ED50 is unreliable.")
  }

  setcolorder(dt, c("y", "n", "x", levs, "y_inc", "y_dec"))
  dt[, c("N", "y1", "y2") := NULL]

  msg <- names(dt)
  setkey(dt, NULL)
  list(
    data = dt,
    levs = levs,
    msg  = msg[str_detect(msg, "msg_")]
  )
}

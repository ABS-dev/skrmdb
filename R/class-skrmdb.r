#' @name skrmdb-class
#' @title skrmdb class
#' @description The functions `ReedMuench()`, `SpearKarb()`, `DragBehr()`, and
#'   `skrmdb.all()` return an S3 object of class `skrmdb`.
#'
#'   The `skrmdb` class object contains the following fields:
#'
#'
#' * `eval`:    The name of the method used to compute the ED50.
#' * `data`:    A `data.frame` of the data used to compute the ED50.
#' * `ed`:      A vector of ED50 values.
#' * `var`:     A vector of variances, only is SpearKarb was used.
#' * `results`: A `data.table` of the results, including brief information
#'  about the data.
#'
#' These components can be accessed via the appropriate accessor functions.
#'
#' @param x An object of class `skrmdb`.
#' @examples
#' y <- c(0, 3, 5, 8, 10, 10)
#' n <- rep(10, 6)
#' x <- 1:6
#' res <- SpearKarb(y + n ~ x)
#' print(res)
#' getED50(res)
#' getVar(res)
#' getData(res)
#' getResults(res)
NULL

#' @rdname skrmdb-class
#' @export
getED50 <- function(x) {
  if ("ed" %in% names(x$results)) {
    x$results$ed
  } else {
    NA_real_
  }
}

#' @rdname skrmdb-class
#' @importFrom lifecycle badge deprecate_warn
#' @usage NULL
#' @export
getED <- function(x) {
  deprecate_warn("4.5.0", "getED50()")
  getED50(x)
}

#' @rdname skrmdb-class
#' @export
getVar <- function(x) {
  if ("var" %in% names(x$results)) {
    x$results$var
  } else {
    NA_real_
  }
}

#' @rdname skrmdb-class
#' @importFrom lifecycle badge deprecate_warn
#' @usage NULL
#' @export
getvar <- function(x) {
  deprecate_warn("4.5.0", "getVar()")
  getED50(x)
}

#' @rdname skrmdb-class
#' @export
getData <- function(x) {
  as.data.frame(x$data)
}

#' @rdname skrmdb-class
#' @usage NULL
#' @importFrom lifecycle deprecate_warn
#' @export
getdata <- function(x) {
  deprecate_warn("5.0.0", "getData()")
  getData(x)
}

#' @rdname skrmdb-class
#'
#' @export
getResults <- function(x) {
  as.data.frame(x$results)
}

#' @title print.skrmdb
#' @description A print generic for a `skrmdb object`
#' @param x An object of class `skrmdb`.
#' @param rnd The number of digits to round the numeric values.
#' @param verbose If `TRUE`, prints out messages to help interpret the last five
#' columns of the printed table.
#' @param ... Additional parameters.
#' @returns The `results` table in `x`, invisibly.
#' @importFrom stringr str_detect str_replace str_to_title str_pad str_dup
#' @importFrom data.table melt := .SD .I .N setnames as.data.table fifelse
#' @importFrom crayon red green
#' @importFrom cli symbol
#' @export
print.skrmdb <- function(x, rnd = 3, verbose = TRUE, ...) {
  Duplicate <- Response <- Dilutions <- Monotonic <- Bracket <- NULL
  y <- data.table(x$results)

  # Format factors and numberic
  nms  <- y[, sapply(.SD, class), .SDcols = names(y)]
  nm_f <- names(nms[nms == "factor"])
  y[, c(nm_f) := lapply(.SD, as.character), .SDcols = nm_f]
  nm_n <- names(nms[nms == "numeric"])
  fmt  <- paste0("%1.", rnd, "f")
  y[, c(nm_n) := lapply(.SD,
                        function(x) sprintf(fmt, round(x, 3))), .SDcols = nm_n]

  # Add a row to the table which contains the column names
  nms <- names(y)
  as.data.table(as.list(nms)) |>
    setnames(nms) |>
    rbind(y) ->
    y

  # Pad the columns for printing
  pad <- function(x, side) {
    if (side == "left") {
      str_pad(x, max(nchar(x)), "left", " ")
    } else {
      nc  <- nchar(x)
      mnc <- max(nc)
      rhs <- (mnc - nc) %/% 2
      lhs <- mnc - nc  - rhs
      tmp <- paste0(str_dup(" ", lhs), x, str_dup(" ", rhs))
      tmp
    }
  }

  nm_o <- setdiff(nms, nm_f)
  y[, c(nm_o) := lapply(.SD, pad, side = "both"), .SDcols = nm_o]
  y[, c(nm_f) := lapply(.SD, pad, side = "left"), .SDcols = nm_f]

  # Color code results
  y[2:.N, Duplicate := fifelse(Duplicate |> trimws() == "No",
                               green(Duplicate),
                               red(Duplicate))]
  y[2:.N, Dilutions := fifelse(Dilutions |> trimws() == "Regular",
                               green(Dilutions),
                               red(Dilutions))]
  y[2:.N, Monotonic := fifelse(Monotonic |> trimws() == "Yes",
                               green(Monotonic),
                               red(Monotonic))]
  y[2:.N, Bracket   := fifelse(Bracket   |> trimws() == "Yes",
                               green(Bracket),
                               red(Bracket))]
  y[2:.N, Response  := fifelse(trimws(Response) == "Increasing" |
                                 x$autosort,
                               green(Response),
                               red(Response))]

  # Print the header
  method <- ifelse(x$eval == "all skrmdb", "methods", "method")
  cat(sprintf("ED50 by the %s %s.\n\n", x$eval, method))

  # Combine all the columns and print as a single string
  value <- V1 <- idx <- NULL
  y[, idx := .I]
  melt(y, id.vars = "idx") |>
    _[, paste(value, collapse = " "), idx] |>
    _[, paste(V1, collapse = "\n")] ->
    display

  cat(display, "\n\n")


  if (verbose) {
    # Print a legend for interpretation
    good <- green(symbol$tick)
    bad  <- red(symbol$cross)


    msg1 <- ifelse(
      y[2:.N, any(str_detect(tolower(Duplicate), "yes"))],
      paste(bad, "Duplicate dilutions detected - counts combined."),
      paste(good, "No duplicate dilutions detected."))
    msg2 <- ifelse(y[2:.N, any(str_detect(Dilutions, "Irregular"))],
                   paste(bad, "Possible missing dilution or",
                         "irregular dilution series detected."),
                   paste(good, "All dilution series appear to be regular."))


    decreasing <- y[2:.N, any(str_detect(Response, "Decreasing"))]
    increasing <- y[2:.N, any(str_detect(Response, "Increasing"))]
    no_change  <- y[2:.N, any(str_detect(Response, "No change"))]
    autosort   <- x$autosort

    msg3 <- if (increasing && !decreasing && !no_change) {
      "All count trends appear to increase as x increases."
    } else if (!increasing && decreasing && !no_change) {
      "All count trends appear to decrease as x increases."
    } else if (!increasing && !decreasing && no_change) {
      "All count trends appear to be constant as x increases."
    } else if (increasing && decreasing && !no_change) {
      paste("Some count trends appear to increase while",
            "others appear to decrease as x increases.")
    } else if (increasing && !decreasing && no_change) {
      paste("Some count trends appear to increase while",
            "others appear to not change as x increases.")
    } else if (!increasing && decreasing && no_change) {
      paste("Some count trends appear to decrease while",
            "others appear to not change as x increases.")
    } else {
      paste("Some count trends appear to increase while",
            "others appear to decrase while",
            "others appear to be constant as x increases.")
    }

    if (decreasing) {
      msg3 <- paste(
        msg3, "\n ",
        if (!autosort) {
          paste("`autosort == FALSE`:",
                "decreasing response ED50 values are suspect.")
        } else {
          paste("`autosort == TRUE`:",
                "decreasing response ED50 values computed using 1-y/n.")
        }
      )
    }

    icon <- ifelse(autosort || !decreasing, good, bad)
    msg3 <- paste(icon, msg3)

    msg4 <- ifelse(y[2:.N, any(str_detect(Monotonic, "No"))],
                   paste(bad, "Some count trends are not monotonic along x."),
                   paste(good, "All count trends are monotonic along x."))
    msg5 <- ifelse(y[2:.N, any(str_detect(Bracket, "No"))],
                   paste(bad, "Some count trends do not bracket ED50."),
                   paste(good, "All count trends bracket ED50.\n"))
    cat(paste(msg1, msg2, msg3, msg4, msg5, sep = "\n"), "\n")
  }

  invisible(data.table(x$results))
}

#' @title Class definition for skrmdb object
#'
#' @description The skrmdb object holds output from functions in the skrmdb
#'   package. A valid skrmdb object is a list which contains the following
#'   components:
#'
#' @param eval Evaluation method. One of `"ReedMuench"`, `"SpearKarb"`, or
#'   `"DragBehr"`. character string.
#' @param lst  A list containing the following information which is used to
#' create the structure
#' * data
#' * results
#' * msg
#' * levs
#' @importFrom data.table setcolorder data.table
#' @noRd
new_skrmdb <- function(
    eval = character(),
    lst) {

  .order_cols <- function(dt) {
    ._tmp__lev <- NULL
    dt[, ._tmp__lev := NULL]
    nms  <- names(dt)
    levs <- intersect(lst$levs, nms)
    msg  <- lst$msg
    vars <- setdiff(nms, c(levs, msg))
    ord  <- c(levs, vars, msg)
    setcolorder(dt, ord)
    dt
  }

  .interpret_msg <- function(dt) {
    msg_duplicate <- msg_response <- msg_dilutions <-
      msg_monotonic <- msg_bracket <- NULL
    # Interpret messages
    dt[, msg_duplicate := fifelse(msg_duplicate, "Yes",        "No")]
    dt[, msg_response  := fifelse(msg_response == 1,  "Increasing",
                                  fifelse(msg_response == -1, "Decreasing",
                                          "No change"))]
    dt[, msg_dilutions := fifelse(msg_dilutions, "Irregular",  "Regular")]
    dt[, msg_monotonic := fifelse(msg_monotonic, "No",         "Yes")]
    dt[, msg_bracket   := fifelse(msg_bracket,   "No",         "Yes")]

    # Change message column names
    msg0 <- names(dt)
    msg0 <- msg0[str_detect(msg0, "^msg_")]
    msg1 <- str_replace(msg0, "^msg_", "") |>
      str_to_title()
    setnames(dt, msg0, msg1)
  }


  data    <- .order_cols(lst$data) |>
    .interpret_msg()
  results <- .order_cols(lst$results) |>
    .interpret_msg()
  ed      <- NA_real_
  if ("ed" %in% names(results)) {
    ed <- results$ed
  }
  var  <- NA_real_
  if ("var" %in% names(results)) {
    var <- results$var
  }

  structure(list(eval     = eval,
                 data     = as.data.frame(data),
                 ed       = ed,
                 var      = var,
                 results  = data.table(results),
                 autosort = lst$autosort),
            class = "skrmdb")
}

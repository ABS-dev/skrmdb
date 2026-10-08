#' @title .skrmdb_all
#'
#' @description Computes ED50 using the three methods found in this package for
#'   a single test.
#'
#' @param dt A `data.table` with the appropriate columns.
#' @param autosort logical.  If TRUE, sorts according to -x or x so that y/n is
#'   increasing with the index
#'
#' @returns a list containing the ED50 for the three methods, the variance as
#'   computed by SpearKarb, and Boolean values for the four warning messages
#'
#' @noRd
.skrmdb_all <- function(dt) {
  db <- .DragBehr(dt)
  rm <- .ReedMuench(dt)
  sk <- .SpearKarb(dt)
  list(DragBehr           =  db$ed,
       ReedMuench         =  rm$ed,
       SpearKarb          =  sk$ed,
       SpearKarb.var      =  sk$var
  )
}

#' @rdname skrmdb
#'
#' @importFrom data.table .SD :=
#' @export
skrmdb.all <- function(
    data,
    formula,
    autosort = TRUE,
    warn.me  = TRUE) {
  y <- n <- x <- msg_response <- NULL
  lst          <- .parse_data(data, formula, y, n, x, autosort, warn.me)
  groups       <- c(lst$levs, lst$msg)
  lst$results  <- lst$data[, .skrmdb_all(.SD), groups] |>
    _[msg_response == 0, SpearKarb := NA]
  lst$autosort <- autosort
  new_skrmdb("all skrmdb", lst)
}

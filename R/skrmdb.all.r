#' \code{.get_opvar}
#'
#' Parses a formula for operators and variables.
#'
#' @param fmla a formula
#'
#' @return a list of two vectors. \code{operators} is a vector of all operators
#'   used in \code{fmla}, and \code{variables} is a vector of all variables used
#'   in \code{fmla}.
#'
#' @noRd
.get_opvar <- function(fmla) {
  opvar <- list(operators = c(),
                variables = c())
  len <- length(fmla)
  if (len == 1) {
    opvar$variables <- as.character(fmla)
  } else {
    opvar$operators <- as.character(fmla[[1]])
    for (ii in 2:len) {
      if (is.name(fmla[[ii]])) {
        opvar$variables <- c(opvar$variables, as.character(fmla[[ii]]))
      } else {
        res <- .get_opvar(fmla[[ii]])
        opvar$operators <- c(opvar$operators, res$operators)
        opvar$variables <- c(opvar$variables, res$variables)
      }
    }
  }
  return(opvar)
}

#' \code{.parse_formula}
#'
#' Parses a formula to determine if it is of one of the following forms \code{y
#' + n ~ x} \code{y + n ~ x | v1} \code{y + n ~ x | v1 + ... + vK}
#'
#' @param fmla a formula
#'
#' @return Logical.  True if \code{fmla} is of the correcto form, FALSE
#'   otherwise.
#'
#' @noRd
.parse_formula <- function(fmla) {
  # LHS must be of form v1 + v2
  opvar.lhs <- .get_opvar(fmla[[2]])
  ops <- opvar.lhs$operators
  if (length(ops) != 1 || !(ops %in% c("cbind", "+")))  {
    return(FALSE)
  }
  # RHS must be of the form v3 or v3 | v4  or V3 | v4 + ... + vK
  if (length(fmla[[3]]) > 1) {
    if (as.character(fmla[[3]][[1]]) != "|" || length(fmla[[3]][[2]]) > 1) {
      return(FALSE)
    }
    ops <- unique(.get_opvar(fmla[[3]][[3]])$operators)
    if (length(ops) > 1 ||
        (length(ops) == 1 && ops != "+")) {
      return(FALSE)
    }
  }
  # all variables names are unique
  all.vars <- c(opvar.lhs$variables, .get_opvar(fmla[[3]])$variables)
  if (length(all.vars) != length(unique(all.vars))) {
    return(FALSE)
  }
  TRUE
}

#' \code{.skrmdb.all}
#'
#' Computes ED50 using the three methods found in this package for a single
#' test.
#'
#' @param y Integer vector.  Number of subjects responding at each level.
#' @param n Integer vector.  Number of subjects tested at each level.
#' @param x Integer vector.  The dilution at each level.
#' @param autosort logical.  If TRUE, sorts according to -x or x so that y/n is
#'   increasing with the index
#'
#' @return a list containing the ED50 for the three methods, the variance as
#'   computed by SpearKarb, and Boolean values for the four warning messages
#'
#' @noRd
.skrmdb.all <- function(y, n, x, autosort = TRUE) {
  A  <- .checkvars(y, n, x, autosort)
  DB <- .DragBehr(A$y_inc, A$y_dec, A$x)
  RM <- .ReedMuench(A$y_inc, A$y_dec, A$x)
  SK <- .SpearKarb(A$y_inc, A$y_dec, A$x, A$n, )
  return(list(DragBehr           =  DB,
              ReedMuench         =  RM,
              SpearKarb          =  SK[1],
              SpearKarb.var      =  SK[2],
              `response increasing` =  attr(A, "warning.increasing"),
              `duplicate dilutions` =  attr(A, "warning.duplicate"),
              `even dilution`       = !attr(A, "warning.uneven"),
              monotonic             = !attr(A, "warning.monotonic"),
              `bracket midpoint`    = !attr(A, "warning.bracket")))
}

#' Function to run all three methods for determining ED50.
#'
#' This function is used primarily by the CVB statistics section to run all
#' three methods included in the \code{\link{skrmdb}} package for determining
#' ED50.
#'
#' @param formula A formula of the form \code{y + n ~ x} or \code{y + n ~ x | v1
#'   + ... + vK}. \code{y} is the number responding at each dilution level,
#'   \code{n} is the number tested at each dilution level, and \code{x} is the
#'   dilution levels.  \code{v1, ..., vK} are the grouping variables.  \code{y,
#'   n, x, v1, ..., vK} must all be distenct.
#' @param data A data.frame containting the titration data. Formatted as
#'   specified in the CVB Data Guide.
#' @param autosort Default \code{TRUE}.  If \code{TRUE} will sort the data
#'   according to either \code{sort(x)} or \code{sort(-x)} so that \code{y / n}
#'   appears to be increasing with the index.  This is how the three methods
#'   assume the data to be ordered.
#'
#' @return A \code{\link{data.frame}} containing columns for each of the subset
#'   variables, the ED50 as computed by \code{DragBehr}, \code{ReedMuench}, the
#'   ED50 and variance as computed by \code{SpearKarb}, and columns of logical
#'   values which indicate if the data are increasing or decreasing with
#'   \code{log_dil}, has an even dilution scheme, are monotonic, and brackets
#'   the midpoint.
#'
#' @examples
#' data(titration)
#' titration$log_dil = -log10(titration$dil)
#' skrmdb.all(positive + total ~ log_dil | Vial + Operator, titration)
#'
#' titration$dil = NULL
#' skrmdb.all(positive + total ~ log_dil | ., titration)
#'
#' @importFrom data.table data.table
#' @importFrom Formula Formula
#' @importFrom stats model.frame
#' @export
#' @export
skrmdb.all <- function(formula, data, autosort = TRUE) {
  y <- n <- x <- NULL
  if (missing(formula))
    stop("skrmdb :: formala is a required parameter.", call. = FALSE)
  if (missing(data))
    stop("skrmdb :: data is a required parameter.", call. = FALSE)
  if (!.parse_formula(formula))
    stop("skrmdb :: formula must be of form y + n ~ x or",
         "y + n ~ x | v1 + ... + vK with y, n, x uniquely named",
         call. = FALSE)
  A <- data.table(model.frame(formula = Formula(formula), data = data))
  setnames(A, names(A)[1:3], c("y", "n", "x"))
  groups <- names(A)[-(1:3)]
  data.frame(A[, .skrmdb.all(y, n, x, autosort), groups])
}

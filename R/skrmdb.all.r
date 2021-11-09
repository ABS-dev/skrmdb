.skrmdb.all <- function(y, n, x, autosort = TRUE) {
  A  <- .checkvars(   y,   n,   x, autosort)
  DB <- .DragBehr(  A$y, A$n, A$x)
  RM <- .ReedMuench(A$y, A$n, A$x)
  SK <- .SpearKarb( A$y, A$n, A$x, A$n0)
  return(list(DragBehr           =  DB,
              ReedMuench         =  RM,
              SpearKarb          =  SK[1],
              SpearKarb.var      =  SK[2],
              increasing         =  attr(A, "warning.increasing"),
              `even dilution`    = !attr(A, "warning.uneven"),
              monotone           = !attr(A, "warning.monotone"),
              `bracket midpoint` = !attr(A, "warning.bracket")))
}

#' Function to run all three methods for determining ED50.
#' 
#' This function is used primarily by the CVB statistics section to run all three methods included
#' in the \code{\link{skrmdb}} package for determining ED50.  
#'  
#' @param x A data frame containing the required columns \code{positive}, \code{total}, and 
#'   \code{log_dil}
#' @param ... The columns which are used to subset the data when calculating ED50 by the three 
#'   methods.  If \code{...} is missing, the code will subset based on all columns that are not
#'   the required ones.  If \code{... == NULL}, the code does not subset the data.
#' @param autosort Default \code{TRUE}.  If \code{TRUE} will sort the data according to either
#'   \code{sort(x)} or \code{sort(-x)} so that \code{y / n} appears to be increasing with the
#'   index.  This is how the three methods assume the data to be ordered.
#'   
#' @return A \code{\link{data.frame}} containing columns for each of the subset variables, the ED50
#'   as computed by \code{DragBehr}, \code{ReedMuench}, the ED50 and variance as computed by 
#'   \code{SpearKarb}, and columns of logical values which indicate if the data are increasing or
#'   decreasing with \code{log_dil}, has an even dilution scheme, are monotone, and brackets the
#'   midpoint.
#'
#' @examples 
#' data(titration)
#' titration$log_dil = -log10(titration$dil)
#' skrmdb.all(titration, Vial, Operator)
#' 
#' titration$dil = NULL
#' skrmdb.all(titration)
#'   
#' @importFrom data.table data.table
#' @export

skrmdb.all <- function(x, ..., autosort = FALSE) {
  positive <- total <- log_dil <- NULL
  colnames <- colnames(x)
  reserved <- c("positive", "log_dil", "total")
  if (!is.data.frame(x)) 
    stop("x must be a data.frame", call. = FALSE)
  if (!("positive" %in% colnames)) 
    stop("required column name 'positive' missing", call. = FALSE)
  if (!("log_dil" %in% colnames))
    stop("required column name 'log_dil' missing", call. = FALSE)
  if (!("total" %in% colnames))
    stop("required column name 'total' missing", call. = FALSE)
  cols <- substitute(list(...))[-1L]
  if (identical(as.character(cols), "NULL")) {
    groups <- c()
  } else if (length(cols)) {
    groups <- unlist(lapply(as.list(cols), as.character), use.names = FALSE)
  } else {
    groups <- colnames
  }
  missing <- setdiff(groups, colnames)
  if (length(missing))
    stop("Column name(s) ", paste0("'", missing, "'", collapse = ", "), " are not found in data",
         call. = FALSE)
  groups <- setdiff(groups, reserved)
  x <- data.table(x)
  data.frame(x[, .skrmdb.all(positive, total, log_dil, autosort), groups])
}



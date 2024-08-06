cat("\014")
rm(list = ls())

library(data.table)
library(Formula)

setGeneric('print')

skrmdb <- setRefClass('skrmdb', 
                      fields = list(ed = 'numeric', eval = 'character'), 
                      methods = list(
                        initialize = function(ed = numeric(), eval = character()) {
                          initFields(ed = ed, eval = eval)
                        }, 
                        print = function() {
                          if(length(.self$ed) == 0) {
                            cat("empty construct\n")
                          } else {
                            cat(paste('ED50 by method ', .self$eval, '\n', sep = ''))
                            cat(.self$ed, '\n')
                          }
                        }
                      ))

setMethod('show', 'skrmdb', function(object) {object$print()})
setMethod('print', 'skrmdb', function(x,...) {x$print()})

setGeneric(name = 'getED', def = function(object) {standardGeneric('getED')})

setMethod('getED', 'skrmdb', function(object) {return(as.numeric(object$ed))})

sk <- setRefClass('sk', 
                  contains = 'skrmdb', 
                  fields = list(sk.var = 'numeric'),
                  methods = list(
                    
                    initialize = function(ed = numeric(), eval = 'SpearKarb', sk.var = numeric()) {
                      if(eval != 'SpearKarb') {
                        
                        stop("[sk: validation] the estimator must be 'SpearKarb'")
                      } else {
                        initFields(ed = ed, eval = eval, sk.var = sk.var)
                      }
                    },
                    print = function() {
                      if(length(.self$ed) == 0) {
                        cat('Empty Construct\n')
                      } else {
                        cat(paste('ED50 by method ', .self$eval, '\n',sep = ''))
                        cat(paste('ed: ', ed = getED(.self), '\n'))
                        cat(paste('sk.var: ', sk.var = .self$sk.var, '\n'))
                      }
                    }
                  ))

setMethod('show', 'sk', function(object) {object$print()})
setMethod('print', 'sk', function(x,...) {x$print()})

.checkdata.old <- function(data, formula) {
  if(!is.null(data) & !is.null(formula)) {
    A <- .checkmatrixorder.old(as.matrix(model.frame(formula = formula, data = data)))
    return(A)
  } else {
    return(NULL)
  }
}

.checkvars.old <- function(y, n, x) {
  if(missing(y) | missing(n) | missing(x)) {
    return(NULL)
  } else if ((length(y) != length(x)) | (length(y) != length(n)) | 
             (length(x) != length(n))) {
    stop('skrmdb :: variables x, n, and y must be the same length')
  } else {
    A <- .checkmatrixorder.old(data.frame(y = y, n = n, x = x))
    
    return(A)  
  }
}

.checkmatrixorder.old <- function(A) {
  if(is.null(rownames(A))) {
    rownames(A) <- 1:nrow(A)
  }
  x <- A[, 3]
  if(!(all(x == cummax(x)) | all(x == cummin(x)))) {
    stop('SpearKarb :: data must be ordered by x variable (either increasing or decreasing)')
  } else {
    y <- A[, 1]
    if(all(y == cummax(y))) {
      # message('skrmdb :: y is monotone increasing')
    } else if(all(y == cummin(y))) {
      # message('skrmdb :: y is monotone decreasing. calculations assume y to be monotone increasing!!')
    } else {
      # warning('skrmdb :: y is not monotone')
    }
  }
  
  return(A)
}


SpearKarb.old.mod <- function(formula = NULL, data = NULL, y, n, x, show = F) {
  A <- .checkdata.old(data = data, formula = formula)
  if(is.null(A)) {
    A <- .checkvars.old(y, n, x)
  }
  y <- A[,1]
  n <- A[,2]
  x <- A[,3]
  
  cum <- y/n
  p <- diff(c(0, cum, 1))
  initial <- 2 * x[1] - x[2]
  final <- 2 * x[length(x)] - x[length(x) - 1]
  D <- apply(rbind(c(x, final), c(initial, x)), 2, mean)
  SK <- sum(p * D)
  p.i <- y/n
  d <- diff(c(initial, x))
  sk.var <- sum((d^2 * p.i * (1 - p.i))/(n - 1))
  if (show) {
    print(A)
    print(x)
    print(y)
    print(n)
    print(round(p.i, 2))
    cat(SK, sk.var, "\n")
    print(data.frame(x, y, n, p = round(p.i, 2)))
  }
  return(new('sk', ed = SK, sk.var = sk.var, eval = "SpearKarb"))
}

DragBehr.old.mod <- function(formula=NULL, data=NULL, y, n, x, warn.me = T, show = F) {
  A <- .checkdata.old(data = data, formula = formula)
  if(is.null(A)) {
    A <- .checkvars.old(y, n, x)
  }
  y <- A[,1]
  n <- A[,2]
  x <- A[,3]
  
  d <- unique(diff(zapsmall(x)))
  p <- y/n
  a <- cumsum(y)
  b <- rev(cumsum(rev(n - y)))
  h <- length(y)
  
  
  if (length(unique(n)) > 1) {
    a <- cumsum(p)
    b <- rev(cumsum(rev(1 - p)))
  }
  P <- a / (a + b)
  diff = zapsmall(P - 0.5)
  if (any(diff == 0)) {
    ed <- x[min(which(diff == 0))]
  } else {
    i <- max(c(1:h)[P <= 0.5])
    ed <- x[i] + ((d * (1/2 - P[i]))/(P[i + 1] - P[i]))
  }
  names(ed) <- 'db'
  if(show)
    print(cbind(y, n, p, a, b, P = round(P, 2), x, d))
  return(new('skrmdb', ed = ed, eval = 'DragBehr'))
}



ReedMuench.old.mod <- function(formula=NULL, data=NULL, y, n, x, warn.me = T, show = F) {
  A <- .checkdata.old(data = data, formula = formula)
  if(is.null(A)) {
    A <- .checkvars.old(y, n, x)
  }
  y <- A[,1]
  n <- A[,2]
  x <- A[,3]
  
  d <- unique(diff(zapsmall(x)))
  h <- length(y)
  p <- y/n
  if(length(d) > 1) {
    d <- x[i + 1] - x[i]
    if(warn.me) warning("Uneven dilution scheme")
  }
  if(length(unique(n)) == 1) {
    a <- cumsum(y)
    b <- rev(cumsum(rev(n - y)))
    P <- a/(a + b)
    diff = zapsmall(P - 0.5)
    if (any(diff == 0)) {
      ed <- x[min(which(diff == 0))]
    } else {
      i <- max(c(1:h)[P <= 0.5])
      ed <- x[i] + (d * (b[i] - a[i]))/(n[i] - y[i] + y[i + 1])
    }
  } else {
    a <- cumsum(p)
    b <- rev(cumsum(rev(1. - p)))
    P <- a/(a + b)
    diff = zapsmall(P - 0.5)
    if (any(diff == 0)) {
      ed <- x[min(which(diff == 0))]
    } else {
      i <- max(c(1:h)[P <= 0.5])
      ed <- x[i] + (d * (b[i] - a[i]))/(1. - p[i] + p[i + 1])
    }
  }
  names(ed) <- 'rm'
  if(show) {
    print(data.frame(y, n, p, a, b, P = round(P, 2), x, d))
  }
  #return(ed)
  return(new('skrmdb', ed = ed, eval = 'ReedMuench'))
}

DragBehr.old = function (formula = NULL, data = NULL, y, n, x, warn.me = T, 
                         show = F) {
  A <- .checkdata.old(data = data, formula = formula)
  if (is.null(A)) {
    A <- .checkvars.old(y, n, x)
  }
  y <- A[, 1]
  n <- A[, 2]
  x <- A[, 3]
  d <- unique(diff(zapsmall(x)))
  p <- y/n
  a <- cumsum(y)
  b <- rev(cumsum(rev(n - y)))
  h <- length(y)
  if (length(unique(n)) > 1) {
    a <- cumsum(p)
    b <- rev(cumsum(rev(1 - p)))
  }
  P <- a/(a + b)
  i <- max(c(1:h)[P <= 0.5])
  if (length(d) > 1) {
    d <- x[i + 1] - x[i]
    if (warn.me) 
      warning("Uneven dilution scheme")
  }
  ed <- x[i] + ((d * (1/2 - P[i]))/(P[i + 1] - P[i]))
  names(ed) <- "db"
  if (show) 
    print(cbind(y, n, p, a, b, P = round(P, 2), x, d))
  return(new("skrmdb", ed = ed, eval = "DragBehr"))
}

ReedMuench.old = function (formula = NULL, data = NULL, y, n, x, warn.me = T, 
                           show = F) {
  A <- .checkdata.old(data = data, formula = formula)
  if (is.null(A)) {
    A <- .checkvars.old(y, n, x)
  }
  y <- A[, 1]
  n <- A[, 2]
  x <- A[, 3]
  d <- unique(diff(zapsmall(x)))
  p <- y/n
  a <- cumsum(y)
  b <- rev(cumsum(rev(n - y)))
  h <- length(y)
  P <- a/(a + b)
  i <- max(c(1:h)[P <= 0.5])
  if (length(d) > 1) {
    d <- x[i + 1] - x[i]
    if (warn.me) 
      warning("Uneven dilution scheme")
  }
  if (length(unique(n)) == 1) 
    ed <- x[i] + (d * (b[i] - a[i]))/(n[i] - y[i] + y[i + 1])
  else {
    A <- cumsum(p)
    B <- rev(cumsum(rev(1 - p)))
    ed <- x[i] + (d * (B[i] - A[i]))/(1 - p[i] + p[i + 1])
  }
  names(ed) <- "rm"
  if (show) 
    print(cbind(y, n, p, a, b, P = round(P, 2), x, d))
  return(new("skrmdb", ed = ed, eval = "ReedMuench"))
}

SpearKarb.old = function (formula = NULL, data = NULL, y, n, x) {
  A <- .checkdata.old(data = data, formula = formula)
  if (is.null(A)) {
    A <- .checkvars.old(y, n, x)
  }
  y <- A[, 1]
  n <- A[, 2]
  x <- A[, 3]
  cum <- y/n
  p <- diff(c(0, cum, 1))
  initial <- 2 * x[1] - x[2]
  final <- 2 * x[length(x)] - x[length(x) - 1]
  D <- apply(rbind(c(x, final), c(initial, x)), 2, mean)
  SK <- sum(p * D)
  p.i <- y/n
  d <- diff(c(initial, x))
  sk.var <- sum((d^2 * p.i * (1 - p.i))/(n - 1))
  return(new("sk", ed = SK, sk.var = sk.var, eval = "SpearKarb"))
}









getED = function(x) {
  return(as.numeric(x$ed))
}

getdata = function(x) {
  return(as.data.frame(A))
}

new_skrmdb <- function(eval = character(), A, 
                       ed = numeric(), 
                       var = NA_real_) {
  errorMessage = NULL
  if (attr(A, "warning.reverse")) {
    errorMessage = "* Order of data was reversed.\n"
  }
  if (attr(A, "warning.uneven")) {
    errorMessage = paste0(errorMessage,
                          "* Uneven dilution scheme.\n")
  }
  if (attr(A, "warning.monotone")) {
    errorMessage = paste0(errorMessage,
                          "* y is not monotonic. ED50 may be unreliable.\n")
  }
  if (attr(A, "warning.bracket")) {
    errorMessage = paste0(errorMessage,
                          "* Dilutions fail to bracket the midpoint. ED50 is unreliable.\n")
  }
  structure(list(A = as.data.frame(A), 
                 eval = eval, 
                 ed = ed, 
                 var = var, 
                 messages = errorMessage),
            class = "skrmdb")
}

print.skrmdb <- function(x, ...) {
  if (length(x$ed) == 0) {
    cat("empty construct\n")
  } else {
    cat(paste("ED50 by the", x$eval, "method\n"))
    cat("ed:  ", signif(x$ed, digits = 3), "\n")
    if (x$eval == "SpearKarb") {
      cat(paste("var: ", signif(x$var, digits = 3), "\n"))
    }
    if (!is.null(x$messages)) {
      cat("\n")
      cat(x$messages)
    }
  }
  invisible(x)
}

.checkmatrix <- function(A, autoarrange, warn.me) {
  A = as.data.table(A)
  setnames(A, c("y", "n", "x"))
  # put in some checking here for what if y, n, or x are not integers
  if (A[, any(c(y, n) != as.integer(c(y, n)))]) {
    stop("skrmdb :: y, n, and x must be integers")
  }
  # Then scale appropriately
  N = A[, prod(unique(n))]
  A[, y0 := y]
  A[, y := y * N / n]
  A[, n0 := n]
  A[, n := N]
  setorder(A, x)
  warning.monotone = warning.uneven = 
    warning.bracket = warning.reverse = FALSE
  if (autoarrange && A[, .N] * A[, sum(y * x)] < A[, sum(x) * sum(y)]) {
    setorder(A, -x)
    if (warn.me) message("skrmdb :: y appears to be decreasing. Reversing order of data.")
    warning.reverse = TRUE
  }
  if (length(unique(diff(zapsmall(A$x)))) > 1) {
    if (warn.me) message("skrmdb :: Uneven dilution scheme.")
    warning.uneven = TRUE
  }
  if (A[, !all(y == cummax(y))] && A[, !all(y == cummin(y))]) {
    if (warn.me) message("skrmdb :: y is not monotonic. ED50 may be unreliable.")
    warning.monotone = TRUE
  }
  if (2 * A[, min(y)] >= N || 2 * A[, max(y)] <= N) {
    if (warn.me) message("skrmdb :: Dilutions fail to bracket the midpoint. ED50 is unreliable.")
    warning.bracket = TRUE
  }
  attr(A, "warning.reverse")  = warning.reverse
  attr(A, "warning.monotone") = warning.monotone
  attr(A, "warning.uneven")   = warning.uneven
  attr(A, "warning.bracket")  = warning.bracket
  return(A)
}

.checkdata <- function(formula, data, autoarrange, wanr) {
  if (is.null(formula)) {
    return(NULL)
  }
  vars <- attr(terms(formula(formula)), which = "variables")
  lhs <- as.character(vars[[2]]) 
  if (!(lhs[1] == "+" || lhs[1] == "cbind") || length(lhs) != 3) {
    stop("skrmdb :: formula must be of form y + n ~ x.")
  }
  A <- as.matrix(model.frame(formula = Formula(formula), data = data))
  if (ncol(A) != 3) {
    stop("skrmdb :: formula must be for form y + n ~ x where y, n, and x are different varaibles.")
  }
  .checkmatrix(A, autoarrange, warn.me)
}

.checkvars <- function(y, n, x, autoarrange, warn.me) {
  # message("Depreciated: Use y + n ~ x.")
  if (missing(y) | missing(n) | missing(x)) {
    return(NULL)
  } else if ((length(y) != length(x)) | (length(y) != length(n))) {
    stop("skrmdb :: variables x, n, and y must be the same length")
  } 
  .checkmatrix(data.table(y = y, n = n, x = x), autoarrange, warn.me)
}

DragBehr <- function(formula = NULL, data = NULL, y, n, x, 
                     autoarrange = TRUE, warn.me = TRUE, show = FALSE) {
  A <- .checkdata(formula, data, autoarrange, warn.me)
  if (is.null(A)) {
    A <- .checkvars(y, n, x, autoarrange, warn.me)
  }
  y <- A$y
  n <- A$n
  x <- A$x
  
  a = cumsum(y)
  b = rev(cumsum(rev(n - y)))
  P <- a / (a + b)
  diff = a - b
  if (any(diff == 0)) {
    ed <- mean(x[which(diff == 0)])
  } else {
    i <- max(which(diff < 0)) 
    ed <- x[i] + (((x[i + 1] - x[i]) * (1 / 2 - P[i])) / (P[i + 1] - P[i]))
  }
  
  if (show) print(data.frame(x, y, n, y0 = A$y0, n0 = A$n0, a, b, P = round(P, 2)))
  return(new_skrmdb("Dragstedt-Behrens", A, ed))
}

ReedMuench <- function(formula = NULL, data = NULL, y, n, x, 
                       autoarrange = TRUE, warn.me = TRUE, show = FALSE) {
  A <- .checkdata(formula, data, autoarrange, warn.me)
  if (is.null(A)) {
    A <- .checkvars(y, n, x, autoarrange, warn.me)
  }
  y <- A$y
  n <- A$n
  x <- A$x
  
  a = cumsum(y)
  b = rev(cumsum(rev(n - y)))
  diff = a - b
  if (any(diff == 0)) {
    ed <- mean(x[which(diff == 0)])
  } else {
    i <- max(which(diff < 0))
    ed <- x[i] + ((x[i + 1] - x[i]) * (b[i] - a[i])) / (n[i] - y[i] + y[i + 1])
  }    
  
  if (show) print(data.frame(x, y, n, y0 = A$y0, n0 = A$n0, a, b, P = round(a / (a + b), 2)))
  return(new_skrmdb("Reed-Muench", A, ed))
}

SpearKarb <- function(formula = NULL, data = NULL, y, n, x, 
                      autoarrange = TRUE, warn.me = TRUE, show = FALSE) {
  A <- .checkdata(formula, data, autoarrange, warn.me)
  if (is.null(A)) {
    A <- .checkvars(y, n, x, autoarrange, warn.me)
  }
  y <- A$y
  n <- A$n
  x <- A$x
  K <- length(x)
  
  p <- diff(c(0, y / n, 1))
  initial <- 2 * x[1] - x[2]
  final <- 2 * x[K] - x[K - 1]
  ed <- 0.5 * sum(p * (c(x, final) + c(initial, x)))
  
  p.i <- y / n
  d <- diff(c(initial, x))
  var <- sum((d^2 * p.i * (1 - p.i)) / (A$n0 - 1))
  
  if (show) print(data.frame(x, y, n, y0 = A$y0, n0 = A$n0, p = round(p.i, 2)))
  return(new_skrmdb("Spearman-Kärber", A, ed, var))
}


# .increment = function(y, n) {
#   for (ii in seq_along(y)) {
#     y[ii] = y[ii] + 1
#     if (y[ii] <= n) break
#     y[ii] = 0
#   }
#   y
# }
# 
# increment = function(y, n) {
#   ll = length(y)
#   repeat {
#     y <- .increment(y, n)
#     if (y[1] < y[ll] && all(y[-ll] <= y[-1])) break
#   }
#   y
# }
# 
# L = vector(mode = "list", length = 2699)
# mN = 5
# mD = 7
# tot = 0
# for (dils in 3:mD) {
#   x = 1:dils
#   for (N in 3:mN) {
#     n = rep(N, dils)
#     y = rep(0, dils)
#     max = rep(N, dils)
#     max[1] = max[1] - 1
#     repeat {
#       y <- increment(y, N)
#       tot <<- tot + 1
#       cat("\r", mD, mN, tot, " ")
#       L[[tot]] = list(y = y, n = n, x = x)
#       if (all(y == max)) break
#     }
#   }
# }
# 
# 
# 
# dt = data.table(Y = "",
#                 X = "",
#                 N = "",
#                 RM.old = rep(0., length(L)),
#                 RM.new = 0.,
#                 RM.new.rev = 0.,
#                 DB.old = 0.,
#                 DB.new = 0.,
#                 DB.new.rev = 0.,
#                 SK.old  = 0.,
#                 SK.new = 0.,
#                 SK.new.rev = 0.)
# 
# 
# for (i in seq_along(L)) {
#   y = paste(L[[i]]$y, collapse = " ")
#   n = paste(L[[i]]$n, collapse = " ")
#   x = paste(L[[i]]$x, collapse = " ")
#   dt[i, "Y"] = y
#   dt[i, "X"] = x
#   dt[i, "N"] = n
# }
# 
# t0 = proc.time()[3]
# for (ii in seq_along(L)) {
#   y = L[[ii]]$y
#   n = L[[ii]]$n
#   x = L[[ii]]$x
#   dt[ii, RM.old := ReedMuench.old.mod(y = y, n = n, x = x)$ed]
# }
# t1 = proc.time()[3]
# for (ii in seq_along(L)) {
#   y = L[[ii]]$y
#   n = L[[ii]]$n
#   x = L[[ii]]$x
#   dt[ii, DB.old := DragBehr.old.mod(y = y, n = n, x = x)$ed]
# }
# t2 = proc.time()[3]
# for (ii in seq_along(L)) {
#   y = L[[ii]]$y
#   n = L[[ii]]$n
#   x = L[[ii]]$x
#   dt[ii, SK.old := SpearKarb.old.mod(y = y, n = n, x = x)$ed]
# }
# t3 = proc.time()[3]
# for (ii in seq_along(L)) {
#   y = L[[ii]]$y
#   n = L[[ii]]$n
#   x = L[[ii]]$x
#   dt[ii, RM.new := ReedMuench(y = y, n = n, x = x, warn = FALSE)$ed]
# }
# t4 = proc.time()[3]
# for (ii in seq_along(L)) {
#   y = L[[ii]]$y
#   n = L[[ii]]$n
#   x = L[[ii]]$x
#   dt[ii, DB.new := DragBehr(y = y, n = n, x = x, warn = FALSE)$ed]
# }
# t5 = proc.time()[3]
# for (ii in seq_along(L)) {
#   y = L[[ii]]$y
#   n = L[[ii]]$n
#   x = L[[ii]]$x
#   dt[ii, SK.new := SpearKarb(y = y, n = n, x = x, warn = FALSE)$ed]
# }
# t6 = proc.time()[3]
# for (ii in seq_along(L)) {
#   y = L[[ii]]$y
#   n = L[[ii]]$n
#   x = L[[ii]]$x
#   dt[ii, RM.new.rev := ReedMuench(y = rev(y), n = rev(n), x = rev(x), warn = FALSE)$ed]
# }
# t7 = proc.time()[3]
# for (ii in seq_along(L)) {
#   y = L[[ii]]$y
#   n = L[[ii]]$n
#   x = L[[ii]]$x
#   dt[ii, DB.new.rev := DragBehr(y = rev(y), n = rev(n), x = rev(x), warn = FALSE)$ed]
# }
# t8 = proc.time()[3]
# for (ii in seq_along(L)) {
#   y = L[[ii]]$y
#   n = L[[ii]]$n
#   x = L[[ii]]$x
#   dt[ii, SK.new.rev := SpearKarb(y = rev(y), n = rev(n), x = rev(x), warn = FALSE)$ed]
# }
# t9 = proc.time()[3]
# 
# times = c(t1 - t0,
#           t2 - t1,
#           t3 - t2,
#           t4 - t3,
#           t5 - t4,
#           t6 - t5,
#           t7 - t6,
#           t8 - t7,
#           t9 - t8)
# 
# dt2 = data.table(time = times,
#                  proc = c("RM", "DB", "SK", "RM", "DB", "SK", "RM", "DB", "SK"),
#                  ver  = c("old", "old", "old", "new", "new", "new", "new.rev", "new.rev", "new.rev"))
# 
# print(dcast(dt2, proc ~ ver, value.var = "time"))
# 
# dt2.tmp = dt2[, .(total = sum(time)), ver]
# dt2.tmp[, ratio := round(total / min(total), 2)]
# 
# print(dt2.tmp)
# 
# t = round(dt2[, sum(time)], 0)
# cat(paste0("\n", t %/% 60, ":", t %% 60, "\n"))
# 
# dt[is.na(RM.old),     RM.old     := -1]
# dt[is.na(RM.new),     RM.new     := -1]
# dt[is.na(RM.new.rev), RM.new.rev := -1]
# 
# dt[is.na(DB.old),     DB.old     := -1]
# dt[is.na(DB.new),     DB.new     := -1]
# dt[is.na(DB.new.rev), DB.new.rev := -1]
# 
# dt[is.na(SK.old),     SK.old     := -1]
# dt[is.na(SK.new),     SK.new     := -1]
# dt[is.na(SK.new.rev), SK.new.rev := -1]
# 
# dt[, RM.diff := round(max(RM.old, RM.new, RM.new.rev) - min(RM.old, RM.new, RM.new.rev), 6), .(Y, X, N)]
# dt[, DB.diff := round(max(DB.old, DB.new, DB.new.rev) - min(DB.old, DB.new, DB.new.rev), 6), .(Y, X, N)]
# dt[, SK.diff := round(max(SK.old, SK.new, SK.new.rev) - min(SK.old, SK.new, SK.new.rev), 6), .(Y, X, N)]
# 
# dt3 = data.table(ED = c("RM", "DB", "SK"),
#                  max.diff = c(dt[, max(RM.diff)],
#                               dt[, max(DB.diff)],
#                               dt[, max(SK.diff)]),
#                  err      = "")
# 
# dt3[max.diff > 0, err := "*"]
# print(dt3)
# 
# # fwrite(dt, file = "skrmdb.check.csv", row.names = TRUE)
# 
# dt[RM.diff != 0 | DB.diff != 0 | SK.diff != 0]


# #All Good
# y = c(0, 1, 2, 3, 4)
# n = c(4, 4, 4, 4, 4)
# x = c(1, 2, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# # monotinicity
# y = c(0, 1, 2, 1, 4)
# n = c(4, 4, 4, 4, 4)
# x = c(1, 2, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# # monotincity + reversed
# y = c(4, 1, 2, 1, 0)
# n = c(4, 4, 4, 4, 4)
# x = c(1, 2, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# # uneven
# y = c(0, 2, 3, 4)
# n = c(4, 4, 4, 4)
# x = c(1, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# # bracket
# y = c(0, 1, 2, 3, 4)
# n = c(8, 8, 8, 8, 8)
# x = c(1, 2, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# y = c(4, 5, 6, 7, 8)
# n = c(8, 8, 8, 8, 8)
# x = c(1, 2, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# # bracket + uneven
# y = c(0, 1, 2, 4)
# n = c(8, 8, 8, 8)
# x = c(1, 2, 3, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# y = c(4, 5, 6, 8)
# n = c(8, 8, 8, 8)
# x = c(1, 2, 3, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# # monotinicity + bracket
# y = c(0, 1, 2, 1, 4)
# n = c(8, 8, 8, 8, 8)
# x = c(1, 2, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# #  + reversed
# y = c(4, 1, 2, 1, 0) + 4
# n = c(8, 8, 8, 8, 8)
# x = c(1, 2, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# # monotinicity + uneven
# y = c(0, 2, 1, 4)
# n = c(4, 4, 4, 4)
# x = c(1, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# #   + reversed
# y = c(4, 1, 2, 0)
# n = c(4, 4, 4, 4)
# x = c(1, 2, 3, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# # monotinicity + uneven + bracket
# y = c(0, 2, 1, 4)
# n = c(4, 4, 4, 4) + 4
# x = c(1, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# y = c(4, 1, 2, 0)
# n = c(4, 4, 4, 4) + 4
# x = c(1, 2, 3, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# y = c(0, 2, 1, 4) + 4
# n = c(4, 4, 4, 4) + 4
# x = c(1, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# y = c(4, 1, 2, 0) + 4
# n = c(4, 4, 4, 4) + 4
# x = c(1, 2, 3, 5)
# SpearKarb(y = y, x = x, n = n)
# 
# # monotinicity
# y = c(0, 1, 2, 1, 4)
# n = c(4, 4, 4, 4, 4)
# x = c(1, 2, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# SpearKarb(y = rev(y), x = rev(x), n = rev(n))
# DragBehr(y = y, x = x, n = n)
# DragBehr(y = rev(y), x = rev(x), n = rev(n))
# ReedMuench(y = y, x = x, n = n)
# ReedMuench(y = rev(y), x = rev(x), n = rev(n))
# 
# y = c(4, 1, 2, 1, 0)
# n = c(4, 4, 4, 4, 4)
# x = c(1, 2, 3, 4, 5)
# SpearKarb(y = y, x = x, n = n)
# SpearKarb(y = rev(y), x = rev(x), n = rev(n))
# DragBehr(y = y, x = x, n = n)
# DragBehr(y = rev(y), x = rev(x), n = rev(n))
# ReedMuench(y = y, x = x, n = n)
# ReedMuench(y = rev(y), x = rev(x), n = rev(n))

HMS = function(x) {
  f = function(x) {
    paste0(paste0(rep(0, 2 - nchar(x)), collapse = ""), x)
  }
  x = round(x, 0)
  HH = x %/% 3600
  MM = (x - HH * 3600) %/% 60
  SS = x %% 60
  paste0(f(HH), ":", f(MM), ":", f(SS))
}

.increment = function(y, n) {
  for (ii in seq_along(y)) {
    y[ii] = y[ii] + 1
    if (y[ii] <= n) break
    y[ii] = 0
  }
  y
}

.increment2 = function(y, n) {
  for (ii in seq_along(y)) {
    y[ii] = y[ii] + 1
    if (y[ii] <= n[ii]) break
    y[ii] = 0
  }
  y
}

k = 0
maxK = 941814
maxK = 961726
num.error = 0

dt = data.table(y = character(maxK),
                n = character(maxK),
                x = character(maxK),
                sk.old = numeric(maxK),
                sk.new = numeric(maxK),
                sk.old.var = numeric(maxK),
                sk.new.var = numeric(maxK),
                db.old = numeric(maxK),
                db.new = numeric(maxK),
                rm.old = numeric(maxK),
                rm.new = numeric(maxK))

sk.diff = 0
sk.var.diff = 0
db.diff = 0
rm.diff = 0

t1 = proc.time()[3]
for (dils in 5:6) {
  x = 1:dils
  xhat = paste0(x, collapse = " ")
  n = rep(0, dils)
  repeat {
    N = n + 4
    nhat = paste0(N, collapse = " ")
    y = rep(0, dils)
    repeat {
      yhat = paste0(y, collapse = " ")
      Y = y * prod(unique(N)) / N
      if (dils * sum(Y * x) > sum(x) * sum(Y)) {
        k <- k + 1
        # sk.o = SpearKarb.old.mod(y = y, x = x, n = N)
        # sk.n = SpearKarb(y = y, x = x, n = N, warn = FALSE)
        # sk.old = sk.o$ed
        # sk.new = sk.n$ed
        # sk.old.var = sk.o$sk.var
        # sk.new.var = sk.n$var
        # 
        # sk.diff = max(abs(sk.old - sk.new), sk.diff)
        # sk.var.diff = max(abs(sk.old.var - sk.new.var), sk.var.diff)
        # if ((is.na(sk.old.var) && !is.na(sk.new.var)) ||
        #     (!is.na(sk.old.var) && is.na(sk.new.var)) ||
        #     (!is.na(sk.old.var) && !is.na(sk.new.var) & zapsmall(sk.old.var) - zapsmall(sk.new.var) != 0)) {
        #   cat("\nSK", y, ":", N, ":", sk.old.var, sk.new.var, "\n")
        #   num.error <- num.error + 1
        #   SpearKarb.old.mod(y = y, x = x, n = N, show = TRUE)$ed
        #   SpearKarb(y = y, x = x, n = N, warn = FALSE, show = TRUE)$ed
        # }

        # db.old = db.new = rm.old = rm.new = NA

        # db.old = DragBehr.old.mod(y = y, x = x, n = N, warn.me = FALSE)$ed
        # db.new = DragBehr(y = y, x = x, n = N, warn = FALSE)$ed
        # 
        # temp = abs(db.old - db.new)
        # if (!is.na(temp) & temp < 0.499) db.diff = max(temp, db.diff)
        # if ((is.na(db.old) && !is.na(db.new)) ||
        #     (!is.na(db.old) && is.na(db.new)) ||
        #     (!is.na(db.old) && !is.na(db.new) & zapsmall(db.old) - zapsmall(db.new) != 0)) {
        #   cat("\nDB", y, ":", N, ":", db.old, db.new, "\n")
        #   num.error <- num.error + 1
        #   DragBehr.old(y = y, x = x, n = N, warn.me = FALSE, show = TRUE)$ed
        #   DragBehr(y = y, x = x, n = N, warn = FALSE, show = TRUE)$ed
        # }

        rm.old = ReedMuench.old(y = y, x = x, n = N, warn.me = FALSE)$ed
        rm.new = ReedMuench(y = y, x = x, n = N, warn = FALSE)$ed
        
        temp = abs(rm.old - rm.new)
        if (!is.na(temp) & temp < 0.499) {
          rm.diff = max(temp, rm.diff)
          if (temp > 0.1) {
            num.error <- num.error + 1
            cat("\n")
            cat(y, "\n")
            cat(N, "\n")
            cat(x, "\n")
          }
        }
        # if ((is.na(rm.old) && !is.na(rm.new)) ||
        #     (!is.na(rm.old) && is.na(rm.new)) ||
        #     (!is.na(rm.old) && !is.na(rm.new) & zapsmall(rm.old) - zapsmall(rm.new) != 0)) {
        #   cat("\nRM", y, ":", N, ":", rm.old, rm.new, "\n")
        #   num.error <- num.error + 1
        #   ReedMuench.old(y = y, x = x, n = N, warn.me = FALSE, show = TRUE)$ed
        #   ReedMuench(y = y, x = x, n = N, warn = FALSE, show = TRUE)$ed
        # }
        # dt[k, c("y", "n", "x", "sk.old", "sk.new", "sk.old.var", "sk.new.var", "db.old", "db.new", "rm.old", "rm.new") :=
        #      .(..yhat, ..nhat, ..xhat, ..sk.old, ..sk.new, ..sk.old.var, ..sk.new.var,  ..db.old, ..db.new, ..rm.old, ..rm.new)]
        # print(dt[k])
        if (k %% 100 == 0) {
          t2 = proc.time()[3] - t1
          t3 = maxK / k * t2
          per = paste0(round(100 * k / maxK, 2), "%")
          cat(HMS(t2), "/", HMS(t3), ":", per, "|", db.diff, rm.diff, sk.diff, sk.var.diff, num.error, "        \r")
        }
        # if (num.error > 0) break
      }
      y = .increment2(y, N)
      if (all(y == 0)) break
      # if (num.error > 0) break
    }
    n = .increment(n, 1)
    if (all(n == 0)) break
    # if (num.error > 0) break
  }
  cat("\n", k, "\n")
  # if (num.error > 0) break
}

print(k)

# cat("\nnum error = ", num.error)

# NO errors!

# dt[is.na(sk.old), sk.old := -Inf]
# dt[is.na(sk.new), sk.new := -Inf]
# dt[is.na(db.old), db.old := -Inf]
# dt[is.na(db.new), db.new := -Inf]
# dt[is.na(rm.old), rm.old := -Inf]
# dt[is.na(rm.new), rm.new := -Inf]
# 
# dt[, sk.old := round(sk.old, 4)]
# dt[, sk.new := round(sk.new, 4)]
# dt[, db.old := round(db.old, 4)]
# dt[, db.new := round(db.new, 4)]
# dt[, rm.old := round(rm.old, 4)]
# dt[, rm.new := round(rm.new, 4)]
# 
# dt[, sk.equal := sk.old == sk.new]
# dt[, db.equal := db.old == db.new]
# dt[, rm.equal := rm.old == rm.new]
# 
# dt[sk.old == -Inf, sk.old := NA]
# dt[sk.new == -Inf, sk.new := NA]
# dt[db.old == -Inf, db.old := NA]
# dt[db.new == -Inf, db.new := NA]
# dt[rm.old == -Inf, rm.old := NA]
# dt[rm.new == -Inf, rm.new := NA]
# 
save(dt, file = "skrmd compare.rdata")
# 
# # Look at agree / disagree numbers
# temp = melt(dt, id.vars = "y", measure.vars = c("sk.equal", "db.equal", "rm.equal"))[, .N, .(variable, value)]
# temp[, Percent := round(100 * N / sum(N), 1), .(variable)]
# temp
# temp = dcast(temp, variable ~ value, value.var = "N")
# temp[, Percent := .(0, 0.3, 0.3)]
# temp
# 
# # Look at DB disagreement
# temp2 = dt[db.equal == FALSE, .N, .(db.old, db.new)]
# setorder(temp2, "db.old", "db.new")
# # temp2
# dt[db.equal == FALSE, db.idx := 1:.N, .(db.old, db.new)]
# dt[db.idx == 1, .(y, n, x, sk.old, sk.new, db.old, db.new)]
# 
# 
# # Look at RM disagreement
# temp3 = dt[rm.equal == FALSE, .N, .(rm.old, rm.new)]
# setorder(temp3, "rm.old", "rm.new")
# temp3
# dt[rm.equal == FALSE, rm.idx := 1:.N, .(rm.old, rm.new)]
# temp4 = dt[rm.idx == 1, .(y, n, x, sk.old, sk.new, rm.old, rm.new)]
# temp4[(is.na(rm.old) & rm.new == round(rm.new, 0)) |
#         (is.na(rm.new) & rm.old == round(rm.old, 0))]

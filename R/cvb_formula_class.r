#' @title CVB_formula Object
#' @description Abstract class for CVB_formulas.
#' @importFrom R6 R6Class
#' @noRd
CVB_formula <- R6Class(
  "CVB_formula",
  private = list(
    params        = NULL,
    init_params = function() {
      private$params                      <- new.env()
      private$params$formula_old          <- formula(y ~ x)
      private$params$formula_new          <- formula(y ~ x)
      private$params$f_names              <- character()
      private$params$g_names              <- character()
      private$params$data_old             <- data.table()
      private$params$data_new             <- data.table()
      private$params$excluded_clusters    <- data.table()
      private$params$excluded_individuals <- data.table()
      private$params$formula_terms        <- data.table()
      private$params$deprecated           <- FALSE
      private$params$match_idx            <- numeric(0)
      private$params$error                <- FALSE
      private$params$error_msg            <- NA_character_
    },
    as_character = function(...) {
      # @description:  Converts a formula to a string.
      vapply(list(...),
             function(formula) {
               formula <- as.character(formula)
               paste(formula[2], formula[1], formula[3])
             }, "")
    },
    expand_dot_operator = function(fmla, dt) {
      target <- "."

      .inject <- function(fmla) {
        for (ii in seq_along(fmla)) {
          if (is.name(fmla[[ii]])) {
            if (fmla[[ii]] == ".") {
              fmla[[ii]] <- target
            }
          } else {
            fmla[[ii]] <- .inject(fmla[[ii]])
          }
        }
        fmla
      }

      if (!is.null(dt)) {
        opvar <- private$get_opvar(fmla)
        ops   <- opvar$operators
        vars  <- opvar$variables |> unique()
        nms   <- names(dt)
        miss  <- setdiff(nms, vars)
        if ("." %in% vars &&
            !"." %in% nms &&
            length(miss) > 0) {
          target <- formula(paste("~", paste(miss, collapse = "+")))[[2]]
          fmla <- .inject(fmla)
        }
      }
      fmla
    },
    #' @importFrom stats reformulate
    replace_fun = function(formula, fun = "cbind", symb = "+") {
      # @description used to replace function calls like
      # * `cbind(x)` with `x`
      # * `cbind(x, y)` with `x + y`
      # * `cbind(x, y, z)` with `x + y + z`
      # * `custer(x)` with `x`
      # * `custer(x, y)` with `x / y`
      # * `custer(x, y, z)` with `x / y / z`
      if (is.name(formula)) {
        return(formula)
      }
      if (as.character(formula[[1]]) != fun) {
        for (i in seq_along(formula)) {
          formula[[i]] <- private$replace_fun(formula[[i]], fun, symb)
        }
        return(formula)
      }
      private$params$deprecated <- TRUE
      len <- length(formula)
      if (len == 1) {
        return(formula)
      }
      if (len == 2) {
        return(private$replace_fun(formula[[2]], fun, symb))
      }
      tmp <- formula[c(1, 2, len)]
      tmp[[1]] <- as.name(symb)
      formula[len] <- NULL
      tmp[[2]] <- formula
      return(private$replace_fun(tmp, fun, symb))
    },
    replace_plus_symbol = function(formula, symbol) {
      # @description Used to replace `+ symbol` with `| symbol`
      # * `a + cbind(...)` becomes `a | cbind(...)`
      # * `a + x/y/x` becomes `a | x/y/z`
      private$params$deprecated <- FALSE
      replace <- function(formula) {
        if (!is.call(formula)) {
          return(formula)
        }
        if ((as.character(formula[[1]]) == "+") &&
            is.call(formula[[3]]) &&
            as.character(formula[[3]][[1]]) == symbol) {
          formula[[1]] <- as.name("|")
          private$params$deprecated <- TRUE
        }
        for (i in seq_along(formula)) {
          formula[[i]] <- replace(formula[[i]])
        }
        formula
      }
      replace(formula)
    },
    #' @importFrom Formula Formula
    reformat = function(formula) {
      # @description Reformat the formulas by removing and replacing deprecated
      #   uses of `cbind()` and `cluster()`
      private$params$formula_old     <- formula
      private$params$formula_old_str <- private$as_character(formula)
      formula |>
        private$replace_plus_symbol("cluster") |>
        private$replace_fun("cbind", "+") |>
        private$replace_fun("cluster", "/") |>
        private$replace_plus_symbol("/") ->
        formula
      private$params$formula_new     <- formula
      private$params$formula_new_str <- private$as_character(formula)
      formula
    },
    add_pair = function(fc, gc) {
      private$params$f_names <- append(private$params$f_names, fc)
      private$params$g_names <- append(private$params$g_names, gc)
    },
    reset_pair = function(fc, gc) {
      private$params$f_names <- character()
      private$params$g_names <- character()
    },
    is_isomorphic = function(formula, g, call = FALSE) {
      # @description Determines if `formula` is isomorphic to `g`.
      #
      # @param formula A part of the user supplied formula
      # @param The function to compare to.
      # @param call If `TRUE` indicates that `formula` is the name of a function
      #   call.  If `FALSE` formula is a parameter to a function call.
      is_series <- function(formula, op = "+", gc = ".", call = FALSE) {
        # @description Checks if the formula is pure sum.  Used in case the
        # match is a `.`. A `.` can match a `.` or a sum of variables.
        if (call) return(as.character(formula) == op)
        if (length(formula) == 1) {
          fc <- as.character(formula)
          is_numeric <- !is.na(suppressWarnings(as.numeric(fc)))
          if (!is_numeric) {
            private$add_pair(fc, gc)
          }
          return(!is_numeric)
        }
        if (length(formula) != 3) return(FALSE)
        for (i in seq_along(formula)) {
          if (!is_series(formula[[i]], op, gc, i == 1)) {
            return(FALSE)
          }
        }
        return(TRUE)
      }

      if (!call && length(g) == 1 && as.character(g) == ".") {
        return(is_series(formula, "+", "."))
      }
      if (!call && length(g) == 1 && as.character(g) == "%") {
        return(is_series(formula, "/", "%"))
      }
      if (length(formula) != length(g)) {
        return(FALSE)
      }
      if (length(g) == 1) {
        fc <- as.character(formula)
        gc <- as.character(g)
        if (call) {
          return(fc == gc)
        } else {
          fn <- suppressWarnings(as.numeric(fc))
          gn <- suppressWarnings(as.numeric(gc))
          if (is.na(fn) != is.na(gn)) {
            return(FALSE)
          }
          if (!is.na(fn) && !is.na(gn)) {
            return(fn == gn)
          }
          private$add_pair(fc, gc)
          return(fc != "." || gc == ".")
        }
      }
      for (i in seq_along(formula)) {
        if (!private$is_isomorphic(formula[[i]], g[[i]], i == 1)) {
          return(FALSE)
        }
      }
      return(TRUE)
    },
    set_factors = function(cols) {
      if (length(cols) == 0) {
        return()
      }
      dt  <- private$params$data_new
      res <- unlist(dt[, lapply(.SD, is.factor), .SDcols = cols])
      res <- names(res[!res])
      dt[, c(res) := lapply(.SD, factor), .SDcols = res]
      private$params$data_new <- dt
    },
    get_opvar = function(fmla) {
      if (inherits(fmla, "Formula")) {
        fmla <- private$as_character(fmla) |> as.formula()
      }
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
            res <- private$get_opvar(fmla[[ii]])
            opvar$operators <- c(opvar$operators, res$operators)
            opvar$variables <- c(opvar$variables, res$variables)
          }
        }
      }
      opvar
    }
  ),
  #' @importFrom stats terms
  #' @importFrom data.table data.table .N
  public = list(
    #' @description Creates a new instance of this class.
    #'
    #'   1. Reformats the formula to replace deprecated uses of
    #'      *   `cbind(v1, v2, ...)` with `v1 + v2 + ...`
    #'      *   `+ cluster(w1, w2, ...)` with `| w1/w2/...`
    #'      *   `+ w1/w2/...` with `| w1/w2/...`
    #'   2. Compares the user supplied `formula` to a list (`...`) of preferred
    #'   formulas.
    #'   3. If there is a match, checks
    #'      *   That all
    #'
    #'   Parses the user supplied `formula` against a preferred formula.
    #' @param formula a formula
    #' @param ... An optional user supplied list of formulas, of which `formula`
    #'   should match at least one of in terms of structure.
    #' @param data A `data.frame` which contains the data to be used in the
    #'   model.
    #' @param deprecate_version Optional.  The version number of the package in
    #'   which this R6 class began to be used.
    #' @importFrom stringr str_replace_all
    #' @importFrom data.table %notin% := setnames .N
    initialize = function(formula,
                          ...,
                          data              = NULL,
                          deprecate_version = NA) {
      user <- N <- NULL
      private$init_params()
      formula <- private$expand_dot_operator(formula, data)
      formula <- private$reformat(formula)
      if (private$params$deprecated) {
        old_str <- paste0("`", private$params$formula_old_str, "`")
        new_str <- paste0("`", private$params$formula_new_str, "`")
        if (!is.na(deprecate_version)) {
          msg <- paste("The formula", old_str,
                       "was deprecated in version",
                       paste0(deprecate_version, "."),
                       "\nUse", new_str, "instead.")
        } else {
          msg <- paste("The formula", old_str,
                       "has been deprecated.\nUse", new_str, "instead.")
        }
        warning(msg, call. = FALSE)
      }
      targets <- list(...)
      if (length(targets) > 0) {
        res <-
          vapply(targets,
                 function(x) {
                   target <- CVB_formula$new(x)$formula
                   private$reset_pair()
                   private$is_isomorphic(formula, target)
                 },
                 logical(1)
          )
        if (!any(res)) {
          msg <- paste0("`", private$as_character(...), "`")
          msg <- str_replace_all(msg, "`%`", "w1/w2/.../wn")
          msg <- paste(msg, collapse = " or ")
          stop("The formula should be of the form ", msg, call. = FALSE)
        }
        res_idx <- min(which(res))
        target  <- targets[[res_idx]]
        private$reset_pair()
        private$is_isomorphic(formula, target)
        target  <- Formula(target)
        formula <- Formula(formula)

        terms <- data.table(
          user   = private$params$f_names,
          target = private$params$g_names
        )
        terms <- unique(terms)
        private$params$formula_terms <- terms
        if (terms[, .N, user][, max(N) != 1] ||
            terms[, .N, target][target %notin% c(".", "%"), max(N) != 1]) {
          stop("The formula should be of the form ",
               private$as_character(target),
               " where all terms are distinct.", call. = FALSE)
        }
        if (!is.null(data)) {
          if (!inherits(data, "data.frame")) {
            stop("The parameter `data` should be a data.frame.", call. = FALSE)
          }
          private$params$data_old <- data.table(data)
          diff <- setdiff(private$params$formula_terms[, user],
                          names(private$params$data_old))
          if (length(diff) > 0) {
            stop("Objects ", paste(paste0("`", diff, "`"), collapse = ", "),
                 " not found in the data.",
                 call. = FALSE)
          }
        }
        data <- tryCatch(
          data.table(model.frame(formula, data, na.action = NULL)),
          error = function(e) {
              vars <- private$get_opvar(as.formula(formula))$variables |>
                unique()
              data <- data.table() |>
                _[, c(vars) := integer(0)]
              data.table(model.frame(formula, data, na.action = NULL))
          }
        )
        terms[target %notin% c(".", "%"), setnames(data, user, target)]
        private$params$data_new <- data
        private$params$excluded_clusters <- data[-seq_len(.N)]
      }
    },
    #' @description Check the data table types.  Any data type that is indicated
    #'   to be a `factor` will be coerced to be a factor in case it is not.
    #' @param ... Named parameters from each element comparison function. Valid
    #'   names are
    #' * `"numeric"`: e.g `y = "numeric"`. The variable `y` can be either
    #'   `numeric` or `integer`.
    #' * `"count"`: e.g. `n = "count"`. The variable `x` can only be
    #'   non-negative integers.
    #' * `"indicator"`: e.g. `y = "indicator"`. The variable `y` can be only
    #'   `0 / 1` OR `FALSE` / `TRUE`
    #' * `"factor"`: e.g. `x = "factor"`.  The variable `x` is either a `factor`
    #'   or a `character`. In the latter case, it is coerced to a `factor`.
    check_types = function(...) {
      target <- NULL
      is_numeric <- function(x) {
        is.numeric(x) || is.integer(x)
      }

      is_indicator <- function(x) {
        all(x %in% c(0, 1))
      }

      is_count <- function(x) {
        if (is_numeric(x)) {
          all(is.na(x) | (x >= 0 & (floor(x) == ceiling(x))))
        } else {
          FALSE
        }
      }

      is_factor <- function(x) {
        is.factor(x) || is.character(x) || is_count(x)
      }

      types <- unlist(list(...))
      valid <- c("indicator", "numeric", "count", "factor")
      if (any(!types %in% valid) > 0) {
        msg <- paste(paste0("\"", valid, "\""), collapse = ", ")
        stop("All types must be one of ", msg, ".", call. = FALSE)
      }

      check_type <- function(type, fctn) {
        sdcols <- types[types == type]
        if (length(sdcols) == 0) {
          return()
        }
        tmp <- private$params$data_new[, .SD, .SDcols = names(sdcols)]
        if (tmp[, .N] > 0) {
          res <- sapply(tmp, fctn)
          if (!all(res)) {
            nm <- names(res[!res])
            user <- private$params$formula_terms[target %in% nm, user]
            if (length(user) == 0) {
              user <- nm
            }
            user <- paste(paste0("\"", user, "\""), collapse = ", ")
            msg  <- paste("The variable(s)", user,
                          "should be of type", type)
            msg <- paste(msg, collapse = ";\n")
            stop(msg, call. = FALSE)
          }
        }
      }
      check_type("indicator", is_indicator)
      check_type("numeric",   is_numeric)
      check_type("count",     is_count)
      check_type("factor",    is_factor)
      private$set_factors(names(types[types == "factor"]))
      TRUE
    },
    #' @description Check multiple relationships between variables.  For example
    #'   `y > 0` or `y <= n`.
    #' @param condition A relationship to check
    #' @importFrom rlang enexpr
    check_relationships = function(condition) {
      # **Issue** Print user formula names, not my preferred names.
      r <- enexpr(condition)
      res <- private$params$data_new[, all(eval(r))]
      if (!res) {
        stop("The relationship ", private$as_character(r),
             " does not hold.", call. = FALSE)
      }
      TRUE
    },
    #' @description Subsets the data set based on a set of criteria
    #' @param condition The criteria to use to subset the data.  Modifies in
    #'   place.
    #' @note Write vignette / example about use of !! operator with this
    #'   function
    #' @importFrom rlang enexpr
    #' @importFrom data.table fsetdiff .N .I
    subset = function(condition) {
      data <- private$params$data_new[eval(enexpr(condition))]
      data[, ._idx := .I]
      data_omit     <- na.omit(data)
      data_excluded <- fsetdiff(data, data_omit)
      data_excluded[, ._idx := NULL]
      if (data_omit[, .N] > 0) {
        data_omit[, ._idx := NULL]
      }
      private$params$excluded_individuals <- data_excluded
      private$params$data_new <-  droplevels(data_omit)
    },
    #' @description Checks that all levels of the treatment group are included
    #'   in the study.  Checks that all clusters / sub-clusters contain elements
    #'   for all clusters and treatment groups.
    #' @param ... A sequence of the cluster names, quoted or unquoted. The first
    #'   element should be the name of column which holds the treatment groups.
    #'   There should be exactly two levels to the treatment. The next names are
    #'   clusters where they are nested left to right, highest to lowest.
    #' @param terms An additional vector of quoted variables to be added to the
    #'   clusters.  Are appended to after ...
    #' @param check_levels If TRUE, check to make sure that the treatment has
    #'   exactly two levels.  If not, stop.
    #' @importFrom data.table .SD setkeyv setcolorder
    check_clusters = function(..., terms = character(), check_levels = FALSE) {
      N <- NULL
      count_levels <- function(x) {
        length(levels(x))
      }

      clusters <- c(as.character(substitute(list(...))[-1L]), terms)
      if (length(clusters) == 0) {
        return(TRUE)
      }

      dt           <- droplevels(private$params$data_new)
      num_clusters <- length(clusters)
      if (check_levels) {
        num_levels   <- dt[, lapply(.SD, count_levels), .SDcols = clusters]
        num_levels   <- unlist(num_levels)
        if (num_levels[1] != 2) {
          stop("The treatment variable should have precisely two levels. ",
               "It appears to have ", num_levels[1], " levels.", call. = FALSE)
        }
      }

      if (num_clusters == 1) {
        return(TRUE)
      }

      last_cluster <- clusters[num_clusters]

      # Check that it is nested
      tmp <- unique(dt[, .SD, .SDcols = clusters[-1]])
      for (idx in num_clusters:2) {
        tmp <- unique(tmp[, .SD, .SDcols = clusters[2:idx]])
        counts <- tmp[, .N, c(clusters[idx])]
        if (counts[, any(N > 1)]) {
          stop("The data do not appear to appear to be from a nested design. ",
               "Check the nesting of the clusters.", call. = FALSE)
        }
      }

      # Check that all nested variables have two outcomes
      tmp <- unique(dt[, .SD, .SDcols = clusters[c(1, num_clusters)]])
      tmp <- tmp[, .N, c(last_cluster)]

      # Remove any strata that deficient.
      tmp[, remove := N != 2]
      tmp[, N := NULL]

      if (tmp[remove == TRUE, .N] > 0) {
        if (check_levels) {
          bad_clusters <- unlist(tmp[remove == TRUE, .SD,
                                     .SDcols = last_cluster])
          bad_clusters <- unique(as.character(sort(bad_clusters)))
          msg <- paste(paste0("'", bad_clusters, "'"), collapse = ", ")
          warning("The cluster(s) ", msg,
                  " do not contain both treatment levels.",
                  " They will be removed from the data.", call. = FALSE)
        }
        setkeyv(dt, last_cluster)
        setkeyv(tmp, last_cluster)
        tmp <- tmp[dt]

        dt  <- tmp[remove == FALSE]
        dt[, remove := NULL]
        setcolorder(dt, clusters)
        dt <- droplevels(dt)

        tmp <- tmp[remove == TRUE]
        tmp[, remove := NULL]
        private$params$excluded_clusters <- tmp
      }

      # Every Strata was deficient.
      if (dt[, .N] == 0 & check_levels) {
        stop("There is no data after removing deficient clusters.",
             call. = FALSE)
      } else {
        private$params$data_new <- dt
      }
      return(TRUE)
    }
  ),
  active = list(
    #' @field formula The reformatted formula.  Read only.
    formula = function() {
      private$params$formula_new
    },
    #' @field data The `data.table` created by `model.frame(formula, data)`. The
    #'   columns are renamed to fit the column names of the preferred syntax
    #'   formula. Read only.
    data    = function() {
      data.table(private$params$data_new)
    },
    #' @field excluded_clusters A `data.table` created by `check_clusters()`
    #'   that contains the data with strata removed from the design due to the
    #'   not having both treatment levels.
    excluded_clusters = function() {
      data.table(private$params$excluded_clusters)
    },
    #' @field excluded_individuals A `data.table` created by `subset()` that
    #'   contains the data with subjects that are removed which belong to the
    #'   subset but have `NA` for a value in the model.
    excluded_individuals = function() {
      data.table(private$params$excluded_individuals)
    },
    #' @field terms A `data.table` which shows the relation between the term
    #'   names in the user supplied formula and the term names in the preferred
    #'   formula.
    terms   = function() {
      data.table(private$params$formula_terms)
    }
  )
)

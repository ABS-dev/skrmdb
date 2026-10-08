find_sub_function <- function(x, name) {
  if (is.function(x)) {
    x <- body(x)
  }
  for (i in seq_along(x)) {
    if (length(x[[i]]) == 3 &&
        inherits(x[[i]], "<-") &&
        is.name(x[[i]][[2]]) &&
        as.character(x[[i]][[2]]) %in% name &&
        is.call(x[[i]][[3]])) {
      return(eval(x[[i]]))
    } else {
      if (length(x[[i]]) > 1) {
        res <- find_sub_function(x[[i]], name)
        if (!is.null(res)) {
          return(res)
        }
      }
    }
  }
  NULL
}

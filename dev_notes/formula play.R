library(Formula)
library(data.table)
library(skrmdb)

data(titration)
titer = data.table(titration)
titer[, log_dil := -log10(dil)]
titer[, dil := NULL]

skrmdb.all(positive + total ~ log_dil | positive, titer)

.get.opvar = function(fmla) {
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
        res = .get.opvar(fmla[[ii]])
        opvar$operators <- c(opvar$operators, res$operators)
        opvar$variables <- c(opvar$variables, res$variables)
      }
    }
  }
  return(opvar)
}


.parse_formula = function(fmla) {
  # LHS must be of form v1 + v2
  opvar.lhs <- .get.opvar(fmla[[2]])
  ops = opvar.lhs$operators
  if (length(ops) != 1 ||
      ops != "+")  {
    return(FALSE)
  } 
  # RHS must be for form v3 or v3 | v4  or V3 | v4 + ... + vK
  if (length(fmla[[3]]) > 1) {
    if (as.character(fmla[[3]][[1]]) != "|" || 
        length(fmla[[3]][[2]]) > 1) {
      return(FALSE)
    }
    ops <- unique(.get.opvar(fmla[[3]][[3]])$operators)
    if (length(ops) > 1 ||
        (length(ops) == 1 && ops != "+")) {
      return(FALSE)
    }
    # vars.rhs <- .get.opvar(fmla[[3]][[2]])$variables
  } 
  # all variables names are unique
  all.vars = c(opvar.lhs$variables, .get.opvar(fmla[[3]])$variables)
  if (length(all.vars) != length(unique(all.vars))) {
    return(FALSE)
  }
  TRUE
}




form = formula(v1 + v2 ~ v3 | v5 + v6 + v6)
.parse_formula(form)
form = formula(v1 + v2 ~ v3 | v5 + v6)
.parse_formula(form)
form = formula(v1 + v2 ~ v3 | v5)
.parse_formula(form)
form = formula(v1 + v2 ~ v2 | v6 + v7)
.parse_formula(form)
form = formula(v1 + v2 ~ v3 | v5 * v6)
.parse_formula(form)
form = formula(v1 + v2 ~ v3)
.parse_formula(form)


get.opvar(form[[3]])
length(form[[3]])
form[[3]][[1]]

form = formula(v1 + v2 ~ v3 + V2)
get.opvar(form[[3]])
length(form[[3]])
form[[3]][[1]]

form = formula(v1 + v2 ~ -v3)
get.opvar(form[[3]])
length(form[[3]])
form[[3]][[1]]

length(form)
form[[1]]; class(form[[1]])
form[[2]]; class(form[[2]])
form[[3]]; class(form[[3]])

length(form[[2]])
form[[2]][[1]]; class(form[[2]][[1]])
form[[2]][[2]]; class(form[[2]][[2]])
form[[2]][[3]]; class(form[[2]][[3]])

length(form[[2]][[2]])
form[[2]][[2]][[1]]; class(form[[2]][[2]][[1]])
form[[2]][[2]][[2]]; class(form[[2]][[2]][[2]])
form[[2]][[2]][[3]]; class(form[[2]][[2]][[3]])


length(form[[3]])
form[[3]][[1]]; class(form[[3]][[1]])
form[[3]][[2]]; class(form[[3]][[2]])
form[[3]][[3]]; class(form[[3]][[3]])

parse_formula(total + bob ~ log_dil)
parse_formula(total + bob ~ log_dil + steve | bob)
parse_formula(total + bob ~ log_dil | bob + steve + george)

form = formula(v1 + v2 + v3 ~ v4 + v5 | v6 | v7)

for (ii in 1:length(form)) {
  cat(length(form[[ii]]), class(form[[ii]]), as.character(form[[ii]]), "\n")
}

class(form)
form[[1]]
form[[2]]
form[[3]]

form
as.character(form[[1]])
as.character(form[[2]])
as.character(form[[3]])
class(form[[1]])
class(form[[2]])
class(form[[3]])
class(form)

attr(terms(form), which = "variables")


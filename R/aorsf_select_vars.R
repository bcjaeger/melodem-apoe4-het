
#' @description
#' helper function for variable selection with aorsf
#'
#' @details
#'  requires aorsf version 0.1.4 or higher!
#'

aorsf_select_vars <- function(data, formula, ...){

  outcome <- as.character(as.list(formula)[[2]])

  fit <- orsf(data = data, formula = formula, ...)
  vars <- orsf_vs(fit, n_predictor_min = 2)
  top <- which.max(vars$stat_value)

  c(outcome, vars$predictors_included[top][[1]])

}

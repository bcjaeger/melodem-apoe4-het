
#' @description
#' helper function for variable selection with aorsf
#'
#' @details
#'  requires aorsf version 0.1.4 or higher!
#'

aorsf_select_vars <- function(data, formula, ...){

  fit <- orsf(data = data, formula = formula, ...)
  vars <- orsf_vs(fit, n_predictor_min = 1)
  top <- which.max(vars$stat_value)
  c(fit$get_names_y(), vars$variables_included[top][[1]])

}

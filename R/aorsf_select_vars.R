
#' @description
#' helper function for variable selection with aorsf
#'
#' @details
#'  requires aorsf version 0.1.4 or higher!
#'


# data <- tar_read(data_sim)$values

aorsf_select_vars <- function(data, formula, ...){

  outcome <- as.character(as.list(formula)[[2]])

  fit <- orsf(data = data, formula = formula, ...)
  vars <- orsf_vs(fit, n_predictor_min = 2)

  top <- which.max(vars$stat_value)

  data_levels <- select(data, where(is.factor)) %>%
    map(levels) %>%
    map2(names(.), ~ paste(.y, .x, sep = "_")) %>%
    enframe(name = 'variable', value = 'term') %>%
    unnest(term)

  selected <- vars$predictors_included[top][[1]] %>%
    enframe(value = 'term') %>%
    select(-name) %>%
    left_join(data_levels) %>%
    mutate(selected = coalesce(variable, term)) %>%
    distinct(selected) %>%
    pull(selected)

  c(outcome, selected)

}

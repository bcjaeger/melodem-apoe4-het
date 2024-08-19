

#' Convert data to format compatible with grf
#'
#' @param data a dataset
#' @param names_X names of the variables to go in X matrix
#' @param name_Y names of the variables to go in Y vector
#' @param name_W names of the variables to go in W vector
#' @param name_D names of the variables to go in D vector
#'
#' @return a list of matrices plus a pre-processing recipe
#'
#' @author bcjaeger
#'
data_coerce_grf <- function(data,
                            names_X = NULL,
                            name_Y = NULL,
                            name_W = NULL,
                            name_D = NULL){

  .name_Y <- name_Y %||% "time"
  .name_D <- name_D %||% "status"
  .name_W <- name_W %||% "treatment"

  stopifnot(is.factor(data[[.name_W]]))

  not_in_X <- c(.name_Y, .name_D, .name_W)

  .names_X <- names_X %||% setdiff(names(data), not_in_X)

  data_x <- select(data, all_of(.names_X))

  recipe_x <- recipe(~ ., data = data_x) %>%
    # step_impute_mode(all_nominal_predictors()) %>%
    # step_impute_mean(all_numeric_predictors()) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_center(all_numeric_predictors()) %>%
    step_scale(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep() %>%
    butcher()

  list(
    recipe_x = recipe_x,
    X = as.matrix(bake(recipe_x, new_data = data_x)),
    Y = data[[.name_Y]],
    W = as.numeric(data[[.name_W]])-1,
    D = data[[.name_D]]
  )

}

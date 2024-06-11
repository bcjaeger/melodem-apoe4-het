

data_coerce_grf <- function(data,
                            names_X = NULL,
                            name_Y = NULL,
                            name_W = NULL,
                            name_D = NULL){

  .name_Y <- name_Y %||% "time"
  .name_D <- name_D %||% "status"
  .name_W <- name_W %||% "apoe4"

  stopifnot(is.factor(data$values[[.name_W]]))

  not_in_X <- c(.name_Y, .name_D, .name_W)

  .names_X <- names_X %||% setdiff(names(data$values), not_in_X)

  data_x <- select(data$values, all_of(.names_X))

  recipe_x <- recipe(~ ., data = data_x) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep() %>%
    butcher()

  list(
    recipe_x = recipe_x,
    X = as.matrix(bake(recipe_x, new_data = data_x)),
    Y = data$values[[.name_Y]],
    W = as.numeric(data$values[[.name_W]])-1,
    D = data$values[[.name_D]]
  )

}


#' @description
#' create simulated data that can be shared
#'

simulate_workshop_data <- function(name,
                                   n_obs = 2000,
                                   seed = 329){

  require(magrittr)
  require(data.table)

  set.seed(seed)

  # number of predictors (does not include apoe4)
  n_predictors <- 5

  # loading
  X <- matrix(rnorm(n_obs * n_predictors), n_obs, n_predictors) %>%
    set_colnames(
      c("age",
        "sex",
        "biomarker_1",
        "biomarker_2",
        "biomarker_3")
    )

  dt <- as.data.table(X)

  fwrite(dt, file = glue('data/{name}-raw.csv'))

  NULL

}

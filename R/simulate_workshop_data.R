

simulate_workshop_data <- function(name,
                                   n_obs = 1000,
                                   seed = 329){

  require(magrittr)
  require(data.table)

  set.seed(seed)

  # number of predictors (does not include apoe4)
  n_predictors <- 5

  # Define the new truncated Y and updated event indicator
  pred_horizon <- 10

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

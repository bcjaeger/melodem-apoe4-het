



data_derive <- function(data){

  UseMethod("data_derive")

}

data_derive.melodem_data <- function(data){

  data

}

data_derive.melodem_sim <- function(data){

  pred_horizon <- 10

  dt <- data$values

  # browser()

  dt[, apoe4_prob := inv_logit(
    biomarker_1/3 + biomarker_2/4 + biomarker_1 * biomarker_3/4
  )]

  dt[, apoe4 := rbinom(.N,
                       size = 1,
                       prob = apoe4_prob)]

  dt[, event_time := rexp(
    .N,
    rate = 0.01 + apoe4 * (biomarker_1 + abs(min(biomarker_1)))
  )]

  dt[, apoe4 := factor(apoe4,
                       levels = c(0, 1),
                       labels = c("normal", "elevated"))]

  dt[, censor_time := runif(.N, 0, pred_horizon * 2/3)]

  dt[, time := pmin(event_time, censor_time)]
  dt[, status := as.integer(event_time < censor_time)]

  data$values <- dt

  data

}


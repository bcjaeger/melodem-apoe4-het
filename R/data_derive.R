



data_derive <- function(data){

  UseMethod("data_derive")

}

data_derive.melodem_data <- function(data){

  data

}

data_derive.sim_1 <- data_derive.sim_2 <- function(data){

  pred_horizon <- 10

  dt <- data$values

  dt[, apoe4_prob := fcase(
    sex == 'male', inv_logit(biomarker_1 * biomarker_3),
    sex == 'female', inv_logit(biomarker_1 + biomarker_2)
  )]

  dt[, apoe4 := rbinom(.N,
                       size = 1,
                       prob = apoe4_prob)]

  dt[, event_time := rexp(.N,
                          rate = apoe4 * abs(biomarker_1) / 5 +
                            abs(biomarker_2) / 5)]

  dt[, apoe4 := factor(apoe4,
                       levels = c(0, 1),
                       labels = c("normal", "elevated"))]

  dt[, censor_time := runif(.N, 0, pred_horizon * 2/3)]

  dt[, time := pmin(event_time, censor_time)]
  dt[, status := as.integer(event_time < censor_time)]

  data$values <- dt

  data

}


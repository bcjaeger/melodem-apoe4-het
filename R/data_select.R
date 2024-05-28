
data_select <- function(data){

  UseMethod("data_select")

}

data_select.melodem_data <- function(data){

  data

}

data_select.sim_1 <- data_select.sim_2 <- function(data){

  dt <- data$values

  dt[, apoe4_prob := NULL]
  dt[, event_time := NULL]
  dt[, censor_time := NULL]

  setcolorder(dt, c("time", "status", "apoe4", "sex"))

  data$values <- dt

  data

}


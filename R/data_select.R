
data_select <- function(data){

  UseMethod("data_select")

}

data_select.melodem_data <- function(data){

  data

}

data_select.melodem_sprint <- function(data){

  unused_outcomes <- data$values %>%
    select(matches("^time|^status"),
           -all_of(c(Sys.getenv("melodem_data_time_var"),
                     Sys.getenv("melodem_data_status_var")))) %>%
    names()

  data$values %<>% select(
    -c(pid,
       all_of(unused_outcomes),
       frailty_ctns,
       site,
       female)
  )

  data

}

data_select.melodem_sim <- function(data){

  dt <- data$values

  dt[, apoe4_prob := NULL]
  dt[, event_time := NULL]
  dt[, censor_time := NULL]

  setcolorder(dt, c("time", "status", "apoe4", "sex"))

  data$values <- dt

  data

}


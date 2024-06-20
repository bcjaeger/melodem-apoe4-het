


data_clean <- function(data){

  UseMethod("data_clean")

}

data_clean.melodem_data <- function(data){

  data

}

data_clean.melodem_sim <- function(data){

  dt <- as.data.table(data$values)

  dt[, age := age * 5 + 65]

  dt[, sex := fifelse(sex > 0, 1, 0)]

  dt[, sex := factor(sex,
                     levels = c(0, 1),
                     labels = c("male", "female"))]

  data$values <- dt

  data

}


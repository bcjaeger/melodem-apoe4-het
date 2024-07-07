
#' @description clean data if needed.
#'
#' At a minimum, make sure your data have time/status for dementia,
#' apoe4, age, and sex, so that you will be able to use your data
#' for the rest of the workshop.
#'
#' @details
#'   By clean, what we really mean is modify existing columns.
#'   To create existing columns, see `R/data_derive.R`
#'


data_clean <- function(data, time_var){

  UseMethod("data_clean")

}

data_clean.melodem_data <- function(data, time_var){

  data$values %<>% data_clean_minimal(time_var)

  data

}


data_clean_minimal <- function(data, time_var){

  data_out <- data

  # be picky about negative times but let time values of 0 go by

  if(any(data_out[[time_var]] < 0 )){
    stop('time column contains negative values but all values should be > 0',
         call. = FALSE)
  }

  if(any(data_out[[time_var]] == 0)){

    warning("time column contains 0's - these will be replaced by 0.01",
            call. = FALSE)

    data_out[[time_var]] %<>% pmax(0.01)

  }

  data_out

}


data_clean.melodem_sim <- function(data, time_var){

  dt <- as.data.table(data$values)

  setDT(dt)

  dt[, age := age * 5 + 65]

  dt[, sex := fifelse(sex > 0, 1, 0)]

  dt[, sex := factor(sex,
                     levels = c(0, 1),
                     labels = c("male", "female"))]

  data$values <- dt

  data

}


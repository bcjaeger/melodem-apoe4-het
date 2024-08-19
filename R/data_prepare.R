
#' @description
#'  run through all the data preparation steps:
#'
#'  1. Load (brings data from file to computing environment)
#'  2. Clean (modify existing columns)
#'  3. Derive (create new columns)
#'  4. Select (drop columns you don't need for analysis)
#'  5. Exclude (restrict analysis to subset of the data)
#'
#' @details
#'   if you don't want to apply one or more of these steps, just
#'   don't write a generic implementation for your data (i.e., do nothing).
#'
#' @author bcjaeger
#'

# targets::tar_load_globals()
# tar_load(file)
# labels <- make_labels()
# trt_var <- Sys.getenv("melodem_data_trt_var")
# time_var <- Sys.getenv("melodem_data_time_var")
# status_var <- Sys.getenv("melodem_data_status_var")

data_prepare <- function(file,
                         labels,
                         trt_var,
                         time_var,
                         status_var,
                         use_lifecourse = FALSE,
                         ...){

  output <- data_load(file) %>%
    data_clean(time_var) %>%
    data_derive() %>%
    data_select() %>%
    data_recode(labels = labels) %>%
    data_rename(trt_var, time_var, status_var) %>%
    data_exclude(...)

  check_names(output$values,
              c("age", "sex", "treatment", "time", "status"))

  if(use_lifecourse) output$values %<>% mutate(time = age + time)

  attr(output, 'lifecourse') <- use_lifecourse

  # in case treatment was passed into data_prepare as a 0/1 numeric.
  if(!is.factor(output$values$treatment)){
    output$values$treatment %<>% as.factor()
  }


  output

}

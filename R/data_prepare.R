
#' @description
#'  run through all the data preparation steps:
#'
#'  1. Load (brings data from file to computing environment)
#'  2. Clean (modify existing columns)
#'  3. Derive (create new columns)
#'  4. Select (drop columns you don't need for analysis)
#'  5. Exclude (restric analysis to subset of the data)
#'
#' @details
#'   if you don't want to apply one or more of these steps, just
#'   don't write a generic implementation for your data (i.e., do nothing).
#'
#' @author bcjaeger
#'

data_prepare <- function(file_name, ...){

  output <- data_load(file_name) %>%
    data_clean() %>%
    data_derive() %>%
    data_select() %>%
    data_exclude(...)

  check_names(output$values,
              c("age", "sex", "apoe4", "time", "status"))

  output

}

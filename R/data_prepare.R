

data_prepare <- function(cohort_name, ...){

  output <- data_load(cohort_name) %>%
    data_clean() %>%
    data_derive() %>%
    data_select() %>%
    data_exclude(...)

  check_names(output$values,
              c("age", "sex", "apoe4", "time", "status"))

  output

}

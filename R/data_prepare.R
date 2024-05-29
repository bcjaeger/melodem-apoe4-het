

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


characteristics_summarize <- function(data_melodem, labels){

  tbl_data <- data_melodem$values

  vars_missing <- setdiff(names(labels$variables), names(tbl_data))

  if(!is_empty(vars_missing)){
    warning("The following variables were missing when we computed ",
            "characteristics for this data set: ",
            glue_collapse(vars_missing, sep = ", ", last = " and "),
            ". If you have these variables in your data but they ",
            "are still flagged by this warning message, you may need ",
            "to change the variable names in your data to match what ",
            "is written in the make_labels function. If you don't ",
            "have these variables in your data, no need to worry.")
  }

  vars_to_select <- names(labels$variables) %>%
    c('treatment') %>%
    unique()

  tbl_data_finalized <- tbl_data %>%
    select(any_of(vars_to_select))

  tbl_summary(tbl_data_finalized,by = 'treatment',
              missing = "no",
              label = as.list(labels$variables)) %>%
    add_overall() %>%
    as_tibble()

}

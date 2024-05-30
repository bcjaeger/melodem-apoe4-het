

data_load <- function(file_path){

  file_name <- file_path %>%
    str_split_1('/') %>%
    last()

  cohort_name <- file_name %>%
    str_remove("-raw.*$")

  file_type <- file_name %>%
    str_remove(paste(cohort_name, 'raw', sep = '-'))

  if(!file_type %in% c('.csv', '.sas7bdat')){
    stop("file type ", file_type, " not supported. Can you convert your file",
         " to a .csv or .sas7bdat type?", call. = FALSE)
  }

  cohort_label <- getOption('cohorts')[cohort_name] %||% cohort_name

  data_input <- switch(file_type,
                       '.csv' = read_csv(file_path),
                       '.sas7bdat' = read_sas(file_path))

  output <- structure(
    .Data = list(
      values = data_input,
      exclusions = tibble(label = glue("{cohort_label} participants"),
                          n_obs = nrow(data_input))
    ),
    class = c(cohort_name, 'melodem_data'),
    label = cohort_label
  )

}







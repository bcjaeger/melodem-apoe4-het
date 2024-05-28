


data_load <- function(cohort_name = 'sim_1'){

  if(cohort_name %in% c('sim_1', 'sim_2')){
    fname <- glue("data/{cohort_name}-raw.csv")
  } else {
    fname <- glue("data/sensitive/{cohort_name}-raw.csv")
  }

  cohort_label <- getOption('cohorts')[cohort_name] %||% cohort_name

  if(!file.exists(fname)){
    stop(glue("{fname} not found - did you save \\
              your data in data/sensitive and name it \\
              {cohort_name}-raw.csv?"))
  }

  data_input <- read_csv(fname)

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







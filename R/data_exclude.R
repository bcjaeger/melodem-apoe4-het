
# for debugging:
# age_min = 55
# age_max = 80

data_exclude <- function(data, age_range = NULL){

  UseMethod("data_exclude")

}

data_exclude.melodem_data <- function(data, age_range = NULL){

  data

}

data_exclude.sim_1 <- data_exclude.sim_2 <- function(data,
                                                     age_range = NULL){

  # demonstrate what an exclusion function would do

  dt <- data$values

  setDT(dt)

  # age_min <= age and age <= age_max
  exclusion_1 <- dt[age %between% age_range %||% c(0, Inf)]

  if(!is.null(age_range)){
    data$exclusions %<>% add_row(
      label = glue("Aged {age_range[1]}-{age_range[2]} years"),
      n_obs = nrow(exclusion_1)
    )
  }

  data$values <- exclusion_1

  data

}


#' @description
#' apply exclusions to melodem data if needed.
#'

data_exclude <- function(data, age_range = NULL){

  UseMethod("data_exclude")

}

data_exclude.melodem_data <- function(data, age_range = NULL){

  if(!is.null(age_range)){

    data$values %<>% filter(age >= min(age_range),
                            age <= max(age_range))

    data$exclusions %<>% add_row(
      label = glue("Aged {min(age_range)}-{max(age_range)} years"),
      n_obs = nrow(data$values)
    )

  }

  data

}

data_exclude.melodem_sim <- function(data,
                                     age_range = NULL){

  # demonstrate what an exclusion function would do

  dt <- data$values

  setDT(dt)

  if(!is.null(age_range)){

    # age_min <= age and age <= age_max
    dt <- dt[age %between% age_range]

    data$exclusions %<>% add_row(
      label = glue("Aged {age_range[1]}-{age_range[2]} years"),
      n_obs = nrow(dt)
    )

  }

  data$values <- dt

  data

}

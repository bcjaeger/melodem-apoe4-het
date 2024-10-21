#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_melodem
#' @return
#' @author bcjaeger
#' @export
create_horizon_grid_time <- function(data_melodem) {

  time_bounds <- quantile(data_melodem$values$time, probs = c(0.10, 0.90))

  horizon_grid_raw <- seq(0, 100, by = 5)

  out <- horizon_grid_raw[ horizon_grid_raw %between% time_bounds ]

  if(is_empty(out)) return(median(data_melodem$values$time))

  out

}

create_horizon_grid_age <- function(data_melodem){

  round_to_nearest_5 <- function(x) {
    round(x / 5) * 5
  }

  time_bounds <- quantile(data_melodem$values$age, probs = c(0.10, 0.90))

  time_bounds_rounded <- round_to_nearest_5(time_bounds)

  horizon_grid_raw <- seq(time_bounds_rounded[1],
                          time_bounds_rounded[2],
                          by = 5)

  out <- horizon_grid_raw[ horizon_grid_raw %between% time_bounds ]

  if(is_empty(out)) return(median(data_melodem$values$age))

  out

}

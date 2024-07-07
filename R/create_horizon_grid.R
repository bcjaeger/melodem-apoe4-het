#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_melodem
#' @return
#' @author bcjaeger
#' @export
create_horizon_grid <- function(data_melodem) {

  time_bounds <- quantile(data_melodem$values$time, probs = c(0.10, 0.90))

  horizon_grid_raw <- seq(0, 100, by = 5)

  out <- horizon_grid_raw[ horizon_grid_raw %between% time_bounds ]

  if(is_empty(out)) return(median(data_melodem$values$time))

  out

}

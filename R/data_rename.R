#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param trt_var
#' @param time_var
#' @param status_var
#' @return
#' @author bcjaeger
#' @export
data_rename <- function(data, trt_var, time_var, status_var) {

  data$values %<>% rename(treatment = !!trt_var,
                          time      = !!time_var,
                          status    = !!status_var)

  data

}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param w_propensity
#' @return
#' @author bcjaeger
#' @export

# for debugging
# data <- tar_read(data_sim)
# w_propensity <- tar_read(fit_aorsf_apoe4) %>%
#   getElement('pred_oobag') %>%
#   .[, 2, drop = TRUE]

fit_grf_surv <- function(data, w_propensity, horizon = NULL) {

  # stop if the data aren't created using data_prep()
  stopifnot(inherits(data, 'melodem_data'))

  data_grf <- data_coerce_grf(data$values)

  if(is.null(horizon)){
    horizon <- median(data_grf$Y)
  }

  fit_grf <- causal_survival_forest(
    X = data_grf$X,
    Y = data_grf$Y,
    W = data_grf$W,
    D = data_grf$D,
    W.hat = w_propensity,
    horizon = horizon,
    target = 'RMST'
  )

  fit_grf

}

#' @title fit a causal random survival forest
#'
#' @description
#'  A helper function to make fitting the causal forest more streamlined.
#'
#' @param data a dataset to fit the causal random forest to. Must be
#'   a melodem class of data.
#'
#' @param fit_orsf an oblique random forest object
#'
#' @param horizon see help page for causal random forest by running
#'  `?grf::causal_random_forest`
#'
#' @param target see help page for causal random forest by running
#'  `?grf::causal_random_forest`
#'
#' @return a causal random forest object
#'
#' @author bcjaeger

# for debugging
# data <- tar_read(data_sim)
# fit_aorsf <- tar_read(fit_aorsf_sim)

fit_grf_surv <- function(data, fit_orsf,
                         horizon = NULL,
                         target = "RMST") {

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
    W.hat = fit_orsf$pred_oobag[, 2],
    horizon = horizon,
    target = target
  )

  fit_grf

}

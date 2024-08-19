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
# data <- tar_read(data_melodem)
# fit_orsf <- tar_read(fit_orsf)

fit_grf_surv <- function(data,
                         trt_random,
                         fit_orsf,
                         horizon = NULL,
                         target = "RMST") {

  # stop if the data aren't created using data_prep()
  stopifnot(inherits(data, 'melodem_data'))

  # drop age column if we are using lifecourse approach
  if(uses_lifecourse(data)) data$values$age <- NULL

  data_grf <- data_coerce_grf(data$values)

  if(is.null(horizon)){
    horizon <- median(data_grf$Y)
  }

  failure_times <- get_failure_times(data_grf)

  W.hat <- if(trt_random == 'yes')
    get_trt_prop(data$values$treatment)
  else
    get_trt_prop(fit_orsf)

  fit_grf <- causal_survival_forest(
    X = data_grf$X,
    Y = data_grf$Y,
    W = data_grf$W,
    D = data_grf$D,
    W.hat = W.hat,
    horizon = horizon,
    target = target,
    failure.times = failure_times
  )

  attr(fit_grf, 'failure_times') <- failure_times

  fit_grf

}

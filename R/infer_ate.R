#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_grf_sim
#' @return
#' @author bcjaeger
#' @export
infer_ate <- function(fit_grf) {

  ate <- average_treatment_effect(fit_grf)

  tibble(ate_est = ate['estimate'],
         ate_lwr = ate['estimate'] - 1.96 * ate['std.err'],
         ate_upr = ate['estimate'] + 1.96 * ate['std.err'])

}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_grf_sim
#' @return
#' @author bcjaeger
#' @export
grf_summarize <- function(fit_grf_sim) {

  ate_smry <- infer_ate(fit_grf_sim)

  blp_smry <- infer_grf_blp(fit_grf_sim) %>%
    nest(.key = 'blp')

  rate_pval <- infer_grf_rate(fit_grf_sim)

  bind_cols(ate_smry, rate_pval, blp_smry)

}

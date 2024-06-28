
#' @description
#' put all of the shareable grf outputs into a single `tibble`
#'
#' @param fit_grf a causal random forest object
#'

grf_summarize <- function(fit_grf) {

  ate_smry <- infer_ate(fit_grf)

  blp_smry <- infer_grf_blp(fit_grf) %>%
    nest(.key = 'blp')

  rate_pval <- infer_grf_rate(fit_grf)

  bind_cols(ate_smry, rate_pval, blp_smry)

}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_grf_sim
#' @return
#' @author bcjaeger
#' @export
infer_grf_blp <- function(fit_grf_sim) {

  blp <- fit_grf_sim %>%
    best_linear_projection(A = fit_grf_sim$X.orig)

  tibble(
    blp_term = rownames(blp),
    blp_est = blp[, "Estimate"],
    blp_lwr = blp[, "Estimate"] - 1.96 * blp[, "Std. Error"],
    blp_upr = blp[, "Estimate"] + 1.96 * blp[, "Std. Error"],
    blp_pval = blp[, "Pr(>|t|)"]
  )

}

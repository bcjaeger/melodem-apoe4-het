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
    term = rownames(blp),
    est = blp[, "Estimate"],
    std_error = blp[, "Std. Error"],
    t_stat = blp[, "t value"],
    p_value = blp[, "Pr(>|t|)"]
  )

}

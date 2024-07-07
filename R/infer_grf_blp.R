#' @title Inferences from grf object
#' @description
#'   the `best_linear_projection()` function from `grf` doesn't return
#'   output that easily converts to a data frame, so this function handles
#'   coercion and tidying.
#'
#' @param fit_grf a causal random forest object
#'
#' @return a `tibble` with grf summary info
#'
#' @author bcjaeger
infer_grf_blp <- function(fit_grf, vars) {

  blp <- fit_grf %>%
    best_linear_projection(A = fit_grf$X.orig[, vars, drop = FALSE])

  tibble(
    blp_term = rownames(blp),
    blp_est = blp[, "Estimate"],
    blp_lwr = blp[, "Estimate"] - 1.96 * blp[, "Std. Error"],
    blp_upr = blp[, "Estimate"] + 1.96 * blp[, "Std. Error"],
    blp_pval = blp[, "Pr(>|t|)"]
  )

}


#' @description
#' put all of the shareable grf outputs into a single `tibble`
#'
#' @param fit_grf a causal random forest object
#'

# fit_grf <- tar_read(fit_grf)$fit[[2]]

grf_summarize <- function(fit_grf, vars = NULL) {

  ate_smry <- infer_ate(fit_grf)

  vars <- vars %||% colnames(fit_grf$X.orig)
  vars_in_X <- vars %in% colnames(fit_grf$X.orig)

  if(!all(vars_in_X)){

    offenders <- which(!vars_in_X)

    stop("selected variables not in X matrix: ", vars[offenders],
         call. = FALSE)

  }

  blp_smry <- infer_grf_blp(fit_grf, vars) %>%
    nest(.key = 'blp')

  rate_pval_overall <- infer_grf_rate(fit_grf)

  rate_pval_apriori <- infer_grf_rate_apriori(fit_grf, vars) %>%
    nest(.key = 'rate_apriori')

  bind_cols(ate_smry, rate_pval_overall, rate_pval_apriori, blp_smry)

}

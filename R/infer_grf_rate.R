
#' @title compute a p-value for AUTOC != 0
#'
#' @description
#'  this test copies the strategy for using cross-validation described
#'  here: https://grf-labs.github.io/grf/articles/rate_cv.html
#'
#'  the idea is to use cross-validation in a way that preserves
#'  independence between the CATE data and the evaluation data.
#'
#' @param fit_grf a generalized random forest object
#' @param n_folds how many cross-validation folds to use (default 10)
#'
#' @return a tibble with a column containing a p-value
#'
#' @author bcjaeger
#'
infer_grf_rate <- function(fit_grf, n_folds = 10) {

  stopifnot(n_folds >= 3)

  X = fit_grf$X.orig
  Y = fit_grf$Y.orig
  W = fit_grf$W.orig
  D = fit_grf$D.orig

  folds = sample(n_folds, size = nrow(X), replace = TRUE)

  samples_by_fold = split(seq_along(folds), folds)

  t.statistics = c()

  # Form AIPW scores for estimating RATE
  overall_scores = get_scores(fit_grf)

  for (k in seq(2, n_folds)) {

    train = unlist(samples_by_fold[1:(k - 1)])

    test = samples_by_fold[[k]]

    cate_forest = causal_forest(X[train, ], Y[train], W[train])

    cate_hat_test = predict(cate_forest, X[test, ])$predictions

    rate_fold = rank_average_treatment_effect.fit(overall_scores[test],
                                                  cate_hat_test)

    t.statistics = c(t.statistics, rate_fold$estimate / rate_fold$std.err)

  }

  p_value = 2 * pnorm(-abs(sum(t.statistics) / sqrt(n_folds - 1)))

  tibble(rate_pval = p_value)



}

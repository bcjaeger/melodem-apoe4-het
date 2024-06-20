#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_grf
#' @return
#' @author bcjaeger
#' @export
infer_grf_rate <- function(fit_grf_sim, n_folds = 10) {

  stopifnot(n_folds >= 3)

  X = fit_grf_sim$X.orig
  Y = fit_grf_sim$Y.orig
  W = fit_grf_sim$W.orig
  D = fit_grf_sim$D.orig

  folds = sample(n_folds, size = nrow(X), replace = TRUE)

  samples_by_fold = split(seq_along(folds), folds)

  t.statistics = c()

  # Form AIPW scores for estimating RATE
  overall_scores = get_scores(fit_grf_sim)

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

  p_value



}

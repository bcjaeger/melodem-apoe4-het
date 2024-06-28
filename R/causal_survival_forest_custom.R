
#' @description
#' a customized verion of grf::causal_survival_forest that allows you
#' to use oblique random forests instead of axis-based random forests to
#' estimate the conditional mean of Y
#'

causal_survival_forest_custom <- function(
    X, Y, W, D,
    W.hat = NULL,
    target = c("RMST", "survival.probability"),
    horizon = NULL,
    failure.times = NULL,
    num.trees = 2000,
    sample.weights = NULL,
    clusters = NULL,
    equalize.cluster.weights = FALSE,
    sample.fraction = 0.5,
    mtry = min(ceiling(sqrt(ncol(X)) + 20), ncol(X)),
    min.node.size = 5,
    honesty = TRUE,
    honesty.fraction = 0.5,
    honesty.prune.leaves = TRUE,
    alpha = 0.05,
    imbalance.penalty = 0,
    stabilize.splits = TRUE,
    ci.group.size = 2,
    tune.parameters = "none",
    compute.oob.predictions = TRUE,
    num.threads = NULL,
    seed = runif(1, 0, .Machine$integer.max)) {

  target <- match.arg(target)
  if (is.null(horizon) || !is.numeric(horizon) || length(horizon) != 1) {
    stop("The `horizon` argument defining the estimand is required.")
  }

  has.missing.values <- grf:::validate_X(X, allow.na = TRUE)
  grf:::validate_sample_weights(sample.weights, X)
  Y <- grf:::validate_observations(Y, X)
  W <- grf:::validate_observations(W, X)
  D <- grf:::validate_observations(D, X)
  clusters <- grf:::validate_clusters(clusters, X)
  samples.per.cluster <- grf:::validate_equalize_cluster_weights(equalize.cluster.weights, clusters, sample.weights)
  num.threads <- grf:::validate_num_threads(num.threads)
  if (any(Y < 0)) {
    stop("The event times must be non-negative.")
  }
  if (!all(D %in% c(0, 1))) {
    stop("The censor values can only be 0 or 1.")
  }
  if (sum(D) == 0) {
    stop("All observations are censored.")
  }
  if (target == "RMST") {
    # f(T) <- min(T, horizon)
    D[Y >= horizon] <- 1
    Y[Y >= horizon] <- horizon
    fY <- Y
  } else {
    # f(T) <- 1{T > horizon}
    fY <- as.numeric(Y > horizon)
  }
  if (is.null(failure.times)) {

    Y.grid <- sort(unique(Y))

    # bcj addition 1
    # large Y.grid can slow computation down.
    # consider simplifying if Y.grid is > 100 points
    if(length(Y.grid) > 100){
      Y.grid <- seq(min(Y.grid), max(Y.grid), length.out = 100)
    }

  } else if (min(Y) < min(failure.times)) {
    stop("If provided, `failure.times` should be a grid starting on or before min(Y).")
  } else {

    # bcj addition 2
    # consider simplifying for computational efficiency
    if(length(failure.times) > 100){
      failure.times.orig <- failure.times
      failure.times <- seq(min(failure.times),
                           max(failure.times),
                           length.out = 100)

      # make the subsetted failure times be a proper subset of the original
      # failure times. It would be more efficient to sample the original,
      # but this helps to ensure the grid is more evenly spaced.
      for(i in seq_along(failure.times)){
        closest_index <- which.min(abs(failure.times[i] - failure.times.orig))
        failure.times[i] <- failure.times.orig[closest_index]
      }

      # in case there were duplicates introduced.
      failure.times <- unique(failure.times)

    }

    Y.grid <- failure.times
  }
  if (length(Y.grid) <= 2) {
    stop("The number of distinct event times should be more than 2.")
  }
  if (horizon < min(Y.grid)) {
    stop("`horizon` cannot be before the first event.")
  }
  if (nrow(X) > 5000 && length(Y.grid) / nrow(X) > 0.1) {
    warning(paste0("The number of events are more than 10% of the sample size. ",
                   "To reduce the computational burden of fitting survival and ",
                   "censoring curves, consider discretizing the event values `Y` or ",
                   "supplying a coarser grid with the `failure.times` argument. "), immediate. = TRUE)
  }

  if (is.null(W.hat)) {
    forest.W <- grf::regression_forest(X, W, num.trees = max(50, num.trees / 4),
                                       sample.weights = sample.weights, clusters = clusters,
                                       equalize.cluster.weights = equalize.cluster.weights,
                                       sample.fraction = sample.fraction, mtry = mtry,
                                       min.node.size = 5, honesty = TRUE,
                                       honesty.fraction = 0.5, honesty.prune.leaves = TRUE,
                                       alpha = alpha, imbalance.penalty = imbalance.penalty,
                                       ci.group.size = 1, tune.parameters = tune.parameters,
                                       compute.oob.predictions = TRUE,
                                       num.threads = num.threads, seed = seed)
    W.hat <- predict(forest.W)$predictions
  } else if (length(W.hat) == 1) {
    W.hat <- rep(W.hat, nrow(X))
  } else if (length(W.hat) != nrow(X)) {
    stop("W.hat has incorrect length.")
  }
  W.centered <- W - W.hat

  args.nuisance <- list(failure.times = failure.times,
                        num.trees = max(50, min(num.trees / 4, 500)),
                        sample.weights = sample.weights,
                        clusters = clusters,
                        equalize.cluster.weights = equalize.cluster.weights,
                        sample.fraction = sample.fraction,
                        mtry = mtry,
                        min.node.size = 15,
                        honesty = TRUE,
                        honesty.fraction = 0.5,
                        honesty.prune.leaves = TRUE,
                        alpha = alpha,
                        prediction.type = "Nelson-Aalen", # to guarantee non-zero estimates.
                        compute.oob.predictions = TRUE,
                        num.threads = num.threads,
                        seed = seed)

  # Compute survival-based nuisance components (https://arxiv.org/abs/2001.09887)
  # m(x) relies on the survival function conditional on only X, while Q(x) relies on the conditioning (X, W).
  # Instead of fitting two separate survival forests, we can use the forest fit on (X, W) to compute m(X)
  # using the identity
  # E[f(T) | X] = e(X) E[f(T) | X, W = 1] + (1 - e(X)) E[f(T) | X, W = 0]
  # (for this to work W has to be binary).

  # orsf would throw an error if columns were unnamed
  if(is.null(colnames(X))) colnames(X) <- paste("x", seq(ncol(X)), sep = "_")

  orsf_data <- as.data.frame(cbind(y = Y, d = D, w = W, X))

  # this is to prevent aorsf from throwing an error when it
  # encounters event times of 0. I should remove this assertion
  # from aorsf, but for now:
  orsf_data$y <- pmax(orsf_data$y, .Machine$double.eps)

  # default is to use unique event times
  if(is.null(args.nuisance$failure.times)){
    args.nuisance$failure.times <- sort(unique(Y[D==1]))
  }

  # plugging in inputs from args.nuisance where possible
  sf.survival <- aorsf::orsf(
    data = orsf_data,
    formula = y + d ~ .,
    n_tree = args.nuisance$num.trees,
    weights = args.nuisance$sample.weights,
    sample_fraction = args.nuisance$sample.fraction,
    mtry = args.nuisance$mtry,
    # using min node size for both leaf stopping criteria.
    # This will usually lead to more shallow trees.
    leaf_min_obs = args.nuisance$min.node.size,
    leaf_min_events = args.nuisance$min.node.size,
    oobag_pred_type = "surv",
    oobag_pred_horizon = args.nuisance$failure.times,
    tree_seeds = round(args.nuisance$seed)
  )

  binary.W <- all(W %in% c(0, 1))

  if (binary.W) {

    # The survival function conditioning on being treated S(t, x, 1) estimated with an "S-learner".
    # Computing OOB estimates for modified training samples is not a workflow we have implemented,
    # so we do it with a manual workaround here (deleting/re-inserting precomputed predictions)

    orsf_data$w <- 1
    S1.hat <- predict(sf.survival, new_data = orsf_data, oobag = TRUE)
    orsf_data$w <- 0
    S0.hat <- predict(sf.survival, new_data = orsf_data, oobag = TRUE)
    orsf_data$w <- W

    if (target == "RMST") {
      Y.hat <- W.hat * grf:::expected_survival(S1.hat, sf.survival$pred_horizon) +
        (1 - W.hat) * grf:::expected_survival(S0.hat, sf.survival$pred_horizon)
    } else {
      horizonS.index <- findInterval(horizon, sf.survival$pred_horizon)
      if (horizonS.index == 0) {
        Y.hat <- rep(1, nrow(X))
      } else {
        Y.hat <- W.hat * S1.hat[, horizonS.index] + (1 - W.hat) * S0.hat[, horizonS.index]
      }
    }

  } else {
    # Ignoring this code branch for the simplicity's sake
    stop("Custom survival models + continuous treatment not implemented")

    # If continuous W fit a separate survival forest to estimate E[f(T) | X].
    # sf.Y <- do.call(grf::survival_forest, c(list(X = X, Y = Y, D = D), args.nuisance))
    # SY.hat <- predict(sf.Y)$predictions
    # if (target == "RMST") {
    #   Y.hat <- expected_survival(SY.hat, sf.Y$failure.times)
    # } else {
    #   horizonS.index <- findInterval(horizon, sf.survival$failure.times)
    #   if (horizonS.index == 0) {
    #     Y.hat <- rep(1, nrow(X))
    #   } else {
    #     Y.hat <- SY.hat[, horizonS.index]
    #   }
    # }
  }

  # The conditional survival function S(t, x, w) used to construct Q(x).
  S.hat <- predict(sf.survival, oobag = TRUE, pred_horizon = Y.grid)

  if (!identical(dim(S.hat), c(length(Y), length(Y.grid)))) stop("Wrong S.hat prediction dims")

  # The conditional survival function for the censoring process S_C(t, x, w).
  orsf_data$d <- 1 - D

  sf.censor <- aorsf::orsf_update(sf.survival,
                                  data = orsf_data,
                                  # default split_min_stat is about 3,
                                  # setting to 10 makes trees more shallow
                                  split_min_stat = 10,
                                  oobag_pred_horizon = Y.grid)

  C.hat <- sf.censor$pred_oobag

  if (!identical(dim(C.hat), c(length(Y), length(Y.grid)))) stop("Wrong C.hat prediction dims")

  if (target == "survival.probability") {
    # Evaluate psi up to horizon
    D[Y > horizon] <- 1
    Y[Y > horizon] <- horizon
  }

  Y.index <- findInterval(Y, Y.grid) # (invariance: Y.index > 0)
  C.Y.hat <- C.hat[cbind(seq_along(Y.index), Y.index)] # Pick out P[Ci > Yi | Xi, Wi]

  if (target == "RMST" && any(C.Y.hat <= 0.05)) {
    warning(paste("Estimated censoring probabilities go as low as:", round(min(C.Y.hat), 5),
                  "- an identifying assumption is that there exists a fixed positive constant M",
                  "such that the probability of observing an event past the maximum follow-up time ",
                  "is at least M (i.e. P(T > horizon | X) > M).",
                  "This warning appears when M is less than 0.05, at which point causal survival forest",
                  "can not be expected to deliver reliable estimates."), immediate. = TRUE)
  } else if (target == "RMST" && any(C.Y.hat < 0.2)) {
    warning(paste("Estimated censoring probabilities are lower than 0.2",
                  "- an identifying assumption is that there exists a fixed positive constant M",
                  "such that the probability of observing an event past the maximum follow-up time ",
                  "is at least M (i.e. P(T > horizon | X) > M)."))
  } else if (target == "survival.probability" && any(C.Y.hat <= 0.001)) {
    warning(paste("Estimated censoring probabilities go as low as:", round(min(C.Y.hat), 5),
                  "- forest estimates will likely be very unstable, a larger target `horizon`",
                  "is recommended."), immediate. = TRUE)
  } else if (target == "survival.probability" && any(C.Y.hat < 0.05)) {
    warning(paste("Estimated censoring probabilities are lower than 0.05",
                  "and forest estimates may not be stable. Using a smaller target `horizon`",
                  "may help."))
  }

  psi <- grf:::compute_psi(S.hat, C.hat, C.Y.hat, Y.hat, W.centered,
                           D, fY, Y.index, Y.grid, target, horizon)
  grf:::validate_observations(psi[["numerator"]], X)
  grf:::validate_observations(psi[["denominator"]], X)

  data <- grf:::create_train_matrices(X,
                                      treatment = W.centered,
                                      survival.numerator = psi[["numerator"]],
                                      survival.denominator = psi[["denominator"]],
                                      censor = D,
                                      sample.weights = sample.weights)

  args <- list(num.trees = num.trees,
               clusters = clusters,
               samples.per.cluster = samples.per.cluster,
               sample.fraction = sample.fraction,
               mtry = mtry,
               min.node.size = min.node.size,
               honesty = honesty,
               honesty.fraction = honesty.fraction,
               honesty.prune.leaves = honesty.prune.leaves,
               alpha = alpha,
               imbalance.penalty = imbalance.penalty,
               stabilize.splits = stabilize.splits,
               ci.group.size = ci.group.size,
               compute.oob.predictions = compute.oob.predictions,
               num.threads = num.threads,
               seed = seed)

  forest <- grf:::do.call.rcpp(grf:::causal_survival_train, c(data, args))
  class(forest) <- c("causal_survival_forest", "grf")
  forest[["seed"]] <- seed
  forest[["_psi"]] <- psi
  forest[["X.orig"]] <- X
  forest[["Y.orig"]] <- Y
  forest[["W.orig"]] <- W
  forest[["D.orig"]] <- D
  forest[["Y.hat"]] <- Y.hat
  forest[["W.hat"]] <- W.hat
  forest[["sample.weights"]] <- sample.weights
  forest[["clusters"]] <- clusters
  forest[["equalize.cluster.weights"]] <- equalize.cluster.weights
  forest[["has.missing.values"]] <- has.missing.values
  forest[["target"]] <- target
  forest[["horizon"]] <- horizon

  forest
}

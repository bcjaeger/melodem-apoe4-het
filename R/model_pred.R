#' @description
#'   These functions are used to compute predictions for
#'   the different learners that are used in the prediction
#'   accuracy benchmark. Each function corresponds to a
#'   specific learner, which may seem unnecessary, but is
#'   actually a nice system to use when the targets package
#'   dictates your computation.
#'
#' @note
#'   After this code was developed, glmnet had a nice release
#'   that included some functions to compute survival probs.
#'   I've checked to make sure the approach I take in
#'   cox_net_pred() below gives similar results as glmnet's
#'   new approach, but I highly recommend future work use the
#'   glmnet:::survfit.coxnet approach.
#'
#' @param object the model object to compute predictions with
#' @param test the data to compute predictions for
#' @param pred_horizon the time(s) to compute predictions at


aorsf_pred <- function(object, test, pred_horizon){


  start_time <- Sys.time()

  res <- predict(object$fit,
                 new_data = test,
                 pred_type = "risk",
                 pred_horizon = pred_horizon)

  end_time <- Sys.time()

  list(
    pred = res,
    time = end_time - start_time
  )

}

cox_net_pred <- function(object, test, pred_horizon){

  res <- matrix(nrow = nrow(test), ncol = length(pred_horizon))


  .test <- model.matrix(~. -1L, data = test) |>
    as_tibble() |>
    select(-time, -status) |>
    as.matrix()

  start_time <- Sys.time()

  lin_preds <- predict(object$fit$glmnet.fit,
                       newx = .test,
                       s = object$fit$lambda.min,
                       type = 'link')

  for(i in seq_along(pred_horizon)){
    res[, i] <- exp(exp(lin_preds) * -object$base_haz[i])
  }

  end_time <- Sys.time()

  list(
    pred = 1 - res,
    time = end_time - start_time
  )

}

cif_pred <- function(object, test, pred_horizon){

  # browser()

  start_time <- Sys.time()

  person_surv_objects <- predict(object$fit, newdata = test, type = 'prob')

  res <- matrix(0, nrow = nrow(test), ncol = length(pred_horizon))

  for(t in seq_along(pred_horizon)){
    res[, t] <- map_dbl(
      .x = person_surv_objects,
      .f = ~ {

        if(min(.x$time > pred_horizon[t])) return(0)

        1 - .x$surv[max(which(.x$time <= pred_horizon[t]))]

      }
    )
  }

  end_time <- Sys.time()

  list(
    pred = res,
    time = end_time - start_time
  )

}

xgb_cox_pred <- function(object, test, pred_horizon){

  .test <- model.matrix(~. -1L, data = select(test, -time, -status))

  res <- matrix(nrow = nrow(test), ncol = length(pred_horizon))

  start_time <- Sys.time()

  lin_preds <- predict(object$fit, newdata = .test, outputmargin = TRUE)

  for(i in seq_along(pred_horizon)){
    res[, i] <- exp(exp(lin_preds) * -object$base_haz[i])
  }

  end_time <- Sys.time()

  list(
    pred = 1-res,
    time = end_time - start_time
  )

}

xgb_aft_pred <- function(object, test, pred_horizon){

  .test <- model.matrix(~. -1L, data = select(test, -time, -status))

  res <- matrix(nrow = nrow(test), ncol = length(pred_horizon))

  start_time <- Sys.time()

  lin_preds <- predict(object$fit, newdata = .test)
  lin_preds <- rescale(-lin_preds, to = c(0.1, 0.99))

  for(i in seq_along(pred_horizon)){
    res[, i] <- lin_preds
  }

  end_time <- Sys.time()

  list(
    pred = res,
    time = end_time - start_time
  )

}

rfsrc_pred <- function(object, test, pred_horizon){

  start_time <- Sys.time()

  res <- predictRisk(object$fit,
                     newdata = test,
                     times = pred_horizon)

  end_time <- Sys.time()

  list(
    pred = res,
    time = end_time - start_time
  )

}

model_pred <- function(object, test, pred_horizon){

  list(
    aorsf   = aorsf_pred(object$aorsf, test, pred_horizon),
    rfsrc   = rfsrc_pred(object$rfsrc, test, pred_horizon),
    cif     = cif_pred(object$cif, test, pred_horizon),
    xgb_aft = xgb_aft_pred(object$xgb_aft, test, pred_horizon),
    cox_net = cox_net_pred(object$cox_net, test, pred_horizon)
  )

}

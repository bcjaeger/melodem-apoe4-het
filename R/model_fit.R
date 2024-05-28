# These functions are used to tune and fit competing learners in the
# aorsf-bench paper. Each function requires training data and some
# specification of node_size, which means different things depending
# on the learner. In addition, functions like cox_net_fit and xgb_cox_fit
# require the prediction horizon to be specified during model fitting
# so that an appropriate estimate of baseline hazard can be stored
# for computing predictions later on.


aorsf_fit <- function(train, node_size = 10, ...){

  mtry <- round(sqrt(ncol(train)-2))

  split_min_events <- min( round(sum(train$status) / 5), 5)

  start_time <- Sys.time()

  fit <- orsf(
    data = train,
    formula = Surv(time, status) ~ .,
    mtry = mtry,
    split_min_obs = node_size,
    split_min_events = split_min_events,
    importance = 'none',
    oobag_pred_type = 'none'
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

rfsrc_fit <- function(train, node_size = 10, ...){

  mtry <- round(sqrt(ncol(train)-2))

  start_time <- Sys.time()


  fit <- rfsrc(Surv(time, status) ~ .,
               ntree = 500,
               samptype = 'swr',
               perf.type = 'none',
               data = train,
               mtry = mtry,
               nodesize = node_size)

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

cif_fit <- function(train, node_size = 10, ...){

  mtry <- round(sqrt(ncol(train)-2))

  start_time <- Sys.time()

  fit <- cforest(Surv(time, status) ~ .,
                 controls = cforest_unbiased(mtry = mtry),
                 data = train)

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

xgb_cox_fit <- function(train,
                        node_size = 10,
                        pred_horizon,
                        ...){

  mtry <- round(sqrt(ncol(train)-2))

  xmat <- model.matrix(~. -1L, data = select(train, -time, -status))

  ymat <- fifelse(
    test = train$status == 1,
    yes = train$time,
    no = train$time * (-1)
  )

  dtrain <- xgb.DMatrix(data = xmat, label = ymat)

  params <- list(eta = 0.01,
                 objective = 'survival:cox',
                 eval_metric = 'cox-nloglik',
                 min_child_weight = node_size,
                 colsample_bynode = mtry / ncol(xmat))

  start_time <- Sys.time()

  cv_fit <- xgb.cv(params = params,
                   data = dtrain,
                   nfold = if(nrow(xmat < 100)) 2 else 5,
                   nround = 5000,
                   early_stopping_rounds = 25,
                   verbose = FALSE)

  fit <- xgb.train(params = params,
                   data = dtrain,
                   nrounds = cv_fit$best_iteration)

  # baseline hazard estimate at pred horizon
  lin_preds <- predict(fit, newdata = xmat, outputmargin = TRUE)

  base_haz <-
    gbm::basehaz.gbm(t = train[, 'time'],
                     delta = train[, 'status'],
                     f.x = lin_preds,
                     t.eval = pred_horizon,
                     smooth = TRUE,
                     cumulative = TRUE)

  end_time <- Sys.time()

  list(fit = fit,
       time_fit = end_time - start_time,
       base_haz = base_haz)

}

xgb_aft_fit <- function(train,
                        node_size = 10,
                        pred_horizon,
                        ...){

  mtry <- round(sqrt(ncol(train)-2))

  xmat <- model.matrix(~. -1L, data = select(train, -time, -status))

  y_lower <- train$time
  y_upper <- fifelse(
    test = train$status == 1,
    yes = train$time,
    no = Inf
  )

  dtrain <- xgb.DMatrix(data = xmat)

  setinfo(dtrain, 'label', y_lower)
  setinfo(dtrain, 'label_lower_bound', y_lower)
  setinfo(dtrain, 'label_upper_bound', y_upper)

  params <- list(eta = 0.01,
                 objective = 'survival:aft',
                 eval_metric = 'aft-nloglik',
                 aft_loss_distribution = 'normal',
                 aft_loss_distribution_scale = 1.20,
                 min_child_weight = node_size,
                 colsample_bynode = mtry / ncol(xmat))

  start_time <- Sys.time()

  cv_fit <- xgb.cv(params = params,
                   data = dtrain,
                   nfold = 5,
                   nround = 5000,
                   early_stopping_rounds = 25,
                   verbose = FALSE)

  fit <- xgb.train(params = params,
                   data = dtrain,
                   nrounds = cv_fit$best_iteration)

  end_time <- Sys.time()

  list(fit = fit,
       time_fit = end_time - start_time)

}


cox_net_fit <- function(train, node_size = 10, pred_horizon, ...){

  xmat <- model.matrix(~. -1,
                       data = select(train, -time, -status))

  ymat <- as.matrix(select(train, time, status))

  start_time <- Sys.time()

  fit <- cv.glmnet(x = xmat,
                   y = ymat,
                   alpha = 1/2,
                   nfolds = 5,
                   family = 'cox')

  # baseline hazard estimate at pred horizon
  lin_preds <- predict(fit$glmnet.fit,
                       newx = xmat,
                       s = fit$lambda.min,
                       type = 'link')

  base_haz <-
    gbm::basehaz.gbm(t = train[, 'time'],
                     delta = train[, 'status'],
                     f.x = lin_preds,
                     t.eval = pred_horizon,
                     smooth = TRUE,
                     cumulative = TRUE)

  end_time <- Sys.time()

  list(fit = fit,
       time_fit = end_time - start_time,
       base_haz = base_haz)

}


model_fit <- function(train, node_size = 10, pred_horizon, ...){

  list(
    aorsf   = aorsf_fit(train, node_size, pred_horizon, ...),
    rfsrc   = rfsrc_fit(train, node_size, pred_horizon, ...),
    cif     = cif_fit(train, node_size, pred_horizon, ...),
    xgb_aft = xgb_aft_fit(train, node_size, pred_horizon, ...),
    cox_net = cox_net_fit(train, node_size, pred_horizon, ...)
  )

}


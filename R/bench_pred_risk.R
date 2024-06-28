
#' @description benchmark experiment with real data
#'
#' @param data dataset to benchmark with
#' @param test_prop what proportion of data to set aside for testing


bench_pred_risk <- function(data,
                            test_prop = 1/2) {

  data_all <- data$values

  test_index <- sample(x = seq(nrow(data_all)),
                       size = round(nrow(data_all) * test_prop),
                       replace = FALSE)

  train <- data_all[-test_index, ]
  test <- data_all[test_index, ]

  imputer <- recipe(x = train, time + status ~ .) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_nzv(all_predictors()) %>%
    step_range(all_numeric_predictors()) %>%
    prep()

  # rfsrc blows up if you give it a tibble
  .train <- as.data.frame(juice(imputer))
  .test <- as.data.frame(bake(imputer, new_data = test))

  event_time_bounds <- quantile(
    x = data_all$time[data_all$status==1],
    probs = c(.25, .75)
  )

  pred_horizon <- sort(unique(.test$time[.test$status == 1]))

  pred_horizon <- pred_horizon[pred_horizon <= event_time_bounds['75%']]
  pred_horizon <- pred_horizon[pred_horizon >= event_time_bounds['25%']]

  if(is_empty(pred_horizon)) return(NULL)

  if(length(pred_horizon) > 30){
    pred_horizon <-
      pred_horizon[floor(seq(1, length(pred_horizon), length.out=30))]
  }

  models <- model_fit(train = .train,
                      pred_horizon = pred_horizon)

  predictions <- model_pred(models, test = .test,
                            pred_horizon = pred_horizon)

  sc <- Score(
    object = map(predictions, ~.x$pred),
    formula = Surv(time, status) ~ 1,
    data = select(test, time, status),
    summary = c('IPA', 'ibs'),
    times = pred_horizon
  )

  cstat <- sc$AUC$score %>%
    group_by(model) %>%
    summarize(cstat = mean(AUC))

  brier <- sc$Brier$score %>%
    group_by(model) %>%
    slice(n()) %>%
    ungroup() %>%
    mutate(
      ibs_scaled = (IBS[model=='Null model'] - IBS) /
        IBS[model=='Null model']
    ) %>%
    select(model, ibs_scaled)

  score <- left_join(cstat, brier)

  out <- score %>%
    mutate(model = as.character(model),
           .before = cstat)

  out

}






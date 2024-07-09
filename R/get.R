

get_trt_prop <- function(x){

  UseMethod('get_trt_prop')

}


get_trt_prop.factor <- function(x){
  stopifnot(length(levels(x)) == 2)
  mean(x == levels(x)[2])
}

get_trt_prop.ObliqueForestClassification <- function(x){
  x$pred_oobag[, 2]
}

get_failure_times <- function(x, max_unique_times){

  UseMethod('get_failure_times')

}

get_failure_times.causal_survival_forest <- function(x, max_unique_times = 25){
  attr(x, 'failure_times')
}

get_failure_times.list <- function(x, max_unique_times = 25){

  failure_times <- unique(x$Y[x$D == 1])

  if(length(failure_times) > max_unique_times){

    probs <- seq(0, max_unique_times) / max_unique_times
    failure_times <- c(0, quantile(failure_times, probs = probs))

  }

  failure_times

}


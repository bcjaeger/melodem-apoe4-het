#' this function creates a tibble that contains information
#' about modeling algos used in the benchmark. It's single
#' purpose is to translate the terms I use for models in my code
#' to the terms that I use in my paper. For example, in my code,
#' I refer to the fast aorsf algo as 'aorsf_fast', but in the
#' paper it is 'aorsf-fast'.

make_model_key <- function() {

  tibble(
    term = c(
      "aorsf_fast",
      "aorsf_cph",
      "aorsf_random",
      "aorsf_net",
      "obliqueRSF",
      "rsfse",
      "rotsf",
      "cif",
      "cox_net",
      "coxtime",
      "xgb_cox",
      "xgb_aft",
      "rfsrc",
      "ranger"
    ),
    label = c(
      "aorsf-fast",
      "aorsf-cph",
      "aorsf-random",
      "aorsf-net",
      "obliqueRSF-net",
      "cif-extension",
      "cif-rotate",
      "cif-standard",
      "glmnet-cox",
      "nn-cox",
      "xgboost-cox",
      "xgboost-aft",
      "rsf-standard",
      "ranger-extratrees"
    ),
  )

}

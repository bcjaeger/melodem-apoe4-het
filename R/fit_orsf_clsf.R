#' @description
#' Fit an oblique random forest to predict probability of treatment status.
#'
#' @param data dataset to be used for fitting the oblique random forest
#' @param select_variables if `TRUE`, variable selection is run. If `FALSE`,
#'   no variable selection is run.
#'
#' @return an oblique random forest object
#'
#' @author bcjaeger

# data <- tar_read(data_melodem)

fit_orsf_clsf <- function(data, select_variables = TRUE) {

  # stop if the data aren't created using data_prep()
  stopifnot(inherits(data, 'melodem_data'))

  # initialize vars to include all predictors (drop dementia outcomes though)
  vars <- setdiff(names(data$values), c("time", "status"))

  # select vars with aorsf if requested
  if(select_variables){

    vars <- aorsf_select_vars(data = data$values,
                              formula = treatment ~ . - time - status,
                              na_action = "impute_meanmode",
                              verbose_progress = TRUE)

  }

  # fit the finalized orsf model
  fit <- orsf(treatment ~ .,
              na_action = "impute_meanmode",
              data = select(data$values, all_of(vars)))

  # all fit functions should return a model
  fit

}

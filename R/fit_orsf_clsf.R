#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @return
#' @author bcjaeger
#' @export
fit_orsf_clsf <- function(data, select_variables = TRUE) {

  # stop if the data aren't created using data_prep()
  stopifnot(inherits(data, 'melodem_data'))

  # initialize vars to include all predictors (drop dementia outcomes though)
  vars <- setdiff(names(data$values), c("time", "status"))

  # select vars with aorsf if requested
  if(select_variables){

    vars <- aorsf_select_vars(data = data$values,
                              formula = apoe4 ~ . - time - status,
                              verbose_progress = TRUE)

  }

  # fit the finalized orsf model
  fit <- orsf(apoe4 ~ ., data = select(data$values, all_of(vars)))

  # all fit functions should return a model
  fit

}

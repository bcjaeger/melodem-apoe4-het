
#' @description not a generic function b/c every dataset should go through
#'   this process of recoding variables to match expectations for models.
#' @param data a melodem dataset
#'
#' @param labels the labels target
#'
#' @author bcjaeger
#' @export
data_recode <- function(data, labels) {

  data$values %<>%
    mutate(sex = factor(sex, levels = names(labels$levels$sex)),
           apoe4 = factor(apoe4, levels = names(labels$levels$apoe4)),
           across(where(is.character), as.factor))

  data

}

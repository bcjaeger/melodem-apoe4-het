
#' @description not a generic function b/c every dataset should go through
#'   this process of recoding variables to match expectations for models.
#' @param data a melodem dataset
#'
#' @param labels the labels target
#'
#' @author bcjaeger
#' @export
data_recode <- function(data, labels) {

  # re-code factor levels that are passed through the labels object
  fctrs <- names(labels$levels) %>%
    intersect(names(data$values))

  for(f in fctrs){
    data$values[[f]] %<>% factor(levels = names(labels$levels[[f]]))
  }

  # set any remaining character variables to factors
  data$values %<>% mutate(across(where(is.character), as.factor))

  data

}

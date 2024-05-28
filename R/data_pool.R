#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ...
#' @return
#' @author bcjaeger
#' @export
data_pool <- function(...) {

  .dots <- list(...)

  if(is_empty(names(.dots)) || any(names(.dots) == "")){
    stop("all arguments must be named.
         Correct: data_pool(name_1 = a, name_2 = b)
         Incorrect: data_pool(a, b)",
         call. = FALSE)
  }

  # convert to factors after pooling
  .dots_data <- map_dfr(.dots, 'values', .id = 'cohort') %>%
    mutate(across(where(is.character), as.factor))

  .dots_exclusions <- .dots %>%
    map_dfr('exclusions', .id = 'cohort') %>%
    pivot_wider(names_from = cohort, values_from = n_obs)

  structure(
    .Data = list(
      values = .dots_data,
      exclusions = .dots_exclusions
    ),
    class = c('melodem_data')
  )


}

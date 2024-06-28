#' @description
#' runs the univariate summary function from aorsf and returns it as a tibble
#'
#' @author bcjaeger
orsf_summarize <- function(fit_orsf) {

  smry_pd <- as.data.table(orsf_summarize_uni(fit_orsf))

  as_tibble(smry_pd)

}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_orsf
#' @return
#' @author bcjaeger
#' @export
orsf_summarize <- function(fit_orsf) {

  smry_pd <- as.data.table(orsf_summarize_uni(fit_orsf))

  smry_pd

}

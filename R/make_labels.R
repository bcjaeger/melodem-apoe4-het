#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author bcjaeger
#' @export
make_labels <- function(...) {

  cohorts <- c(
    sim = "Simulated data"
  )

  variables <- c(
    age = "Age",
    sex = "Sex",
    apoe4 = "Apolipoprotein E"
  )

  units <- c(
    age = "years"
  )

  levels <- list(
    sex = c("male" = "Men", "female" = "Women"),
    apoe4 = c("non_carrier" = "Non carrier", "carrier" = "Carrier")
  )

  abbreviations <- c(
    CI   = 'confidence interval',
    SD   = 'standard deviation',
    IQR  = 'interquartile range',
    MoCA = 'Montreal cognitive assessment',
    EHR  = 'electronic health records',
    CVD  = 'cardiovascular disease',
    CKD  = 'chronic kidney disease'
  )

  definitions <- list(
    ckd = paste(
      "estimated glomerular filtration rate <60 ml/min/1.73",
      "meters squared based on the 2021 CKD-EPI creatinine equation."
    )
  )

  labels <- list(...)

  labels$cohorts       <- cohorts
  labels$variables     <- variables
  labels$units         <- units
  labels$levels        <- levels
  labels$abbreviations <- abbreviations
  labels$definitions   <- definitions

  labels

}

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
    # apoe4 = "Apolipoprotein E",
    treatment = "Apolipoprotein E",
    sbp = "Systolic blood pressure",
    dbp = "Diastolic blood pressure",
    stroke = "Stroke", # added by Emma 1/22
    heart = "Heart conditions", # added by Emma 1/22
    hypertension = "Hypertension", # added by Emma 1/22
    diabetes = "Diabetes",
    egfr = "Estimated GFR",
    race = "Race/Ethnicity",
    BMI = "Body mass index",
    smoking = "Smoking status",
    smokesta = "Smoking status", # added by Emma 1/22
    physical.activity = "Physical activity",
    vigeractiv = "Physical activity (Vigorous vs. other)", # added by Emma 1/22
    education = "Education"
  )

  units <- c(
    age = "years",
    sbp = "mmHg",
    dbp = "mmHg",
    egfr= "ml/min/1.73m2"
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
    CKD  = 'chronic kidney disease',
    GFR  = 'glomerular filtration rate'
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

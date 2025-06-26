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
    apoe4 = "Apolipoprotein E",
    treatment = "Apolipoprotein E",
    BMI = "Body mass index",
    FBMI = "Body mass index",
    diabetes = "Diabetes",
    f_dia = "Diabetes",
    dbp = "Diastolic blood pressure",
    DEM06 = "Education",
    education = "Education",
    egfr = "Estimated GFR",
    heart = "Heart conditions",
    f_hyp = "Hypertension",
    hypertension = "Hypertension",
    mi = "Myocardial infarction",
    occup = "Occupation",
    physical.activity = "Physical activity",
    vigeractiv = "Physical activity (Vigorous vs. other)",
    race = "Race/Ethnicity",
    raceeth = "Race/Ethnicity",
    sex = "Sex",
    f_smoke = "Smoking status",
    smokesta = "Smoking status",
    smoking = "Smoking status",
    stroke = "Stroke",
    sbp = "Systolic blood pressure",
    income_first = "income"
  )




  units <- c(
    age = "years",
    sbp = "mmHg",
    dbp = "mmHg",
    egfr= "ml/min/1.73m2"
  )

  levels <- list(
    sex = c("male" = "Men",
            "female" = "Women"),
    apoe4 = c("non_carrier" = "Non carrier",
              "carrier" = "Carrier"),
    race = c("White" = "White",
             "Black" = "Black",
             "Asian" = "Asian",
             "Hispanic" = "Hispanic",
             "Other" = "Other")
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

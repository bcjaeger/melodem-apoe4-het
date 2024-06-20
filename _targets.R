

# manage packages ---------------------------------------------------------

# these need to be loaded when the pipeline starts
library(targets)
library(tarchetypes)
library(magrittr)
library(tibble)
library(glue)
library(here)

tar_option_set(
  # these only need to be loaded when targets are being made
  packages = c(
    "tidyverse",  # general data management
    "tidymodels", # general data management
    "data.table", # general data management
    "haven",      # reading sas files
    "magrittr",   # pipes!
    "glue",       # string management
    "grf",        # causal random forests
    "aorsf",      # oblique random forests
    "glmnet",     # penalized regression
    "xgboost",    # boosted trees
    "randomForestSRC", # axis based random forests
    "party",           # conditional inference forests
    "riskRegression",  # evaluates prediction accuracy
    "survival",        # provides the Surv() function
    "ggforce",         # graphics
    "rpart",           # decision trees
    "ranger",          # random forests
    "butcher"          # manage memory
  )
)


# Load R functions --------------------------------------------------------

lapply(list.files("./R", full.names = TRUE), source)


# Assertions --------------------------------------------------------------

assert_data_safety()

# Globals -----------------------------------------------------------------

manuscript_version <- 1

# slides targets ----------------------------------------------------------

# don't worry about making these on new computers
penguin_figs_tar <- tar_target(penguin_figs, viz_penguins(),
                               cue = tar_cue('never'))

# Individual cohort targets -----------------------------------------------

file_sim_tar <- tar_target(
  file_sim,
  command = "data/sim-raw.csv",
  format = 'file'
)

data_sim_tar <- tar_target(
  data_sim,
  data_prepare(file_sim)
)

# real data cohorts (to be added as an exercise)



# Model targets -----------------------------------------------------------

fit_aorsf_sim_tar <- tar_target(
  fit_aorsf_sim,
  fit_orsf_clsf(data = data_sim)
)

fit_grf_sim_tar <- tar_target(
  fit_grf_sim,
  fit_grf_surv(data = data_sim,
               w_propensity = fit_aorsf_sim$pred_oobag[,1])
)

# real data model targets (to be added as an exercise)

# Shar-eable targets ------------------------------------------------------

pd_smry_sim_tar <- tar_target(
  pd_smry_sim,
  as.data.table(orsf_summarize_uni(fit_aorsf_sim))
)

rate_pval_tar <- tar_target(
  rate_pval,
  infer_grf_rate(fit_grf_sim)
)

blp_smry_tar <- tar_target(
  blp_smry,
  infer_grf_blp(fit_grf_sim)
)


# real data share-able targets (to be added as an exercise)


# Manuscript targets ------------------------------------------------------

if(!dir.exists(glue("manuscript/manuscript-v{manuscript_version}"))){
  dir.create(glue("manuscript/manuscript-v{manuscript_version}"))
}

manuscript_tar <- tar_render(
  manuscript,
  path = here::here("manuscript/manuscript.Rmd"),
  output_file = paste0("manuscript", "-v", manuscript_version, "/",
                       "manuscript-", basename(here()),
                       "-v", manuscript_version,
                       ".docx"),
  packages = c("officer",
               "flextable",
               "table.glue",
               "gtsummary")
)


# Finalize targets --------------------------------------------------------

targets <- list(
  penguin_figs_tar,
  file_sim_tar,
  data_sim_tar,
  fit_aorsf_sim_tar,
  fit_grf_sim_tar,
  pd_smry_sim_tar,
  rate_pval_tar
)


# hook not needed for tar_make() but is for tar_make_future().
tar_hook_before(
  targets = targets,
  hook = {
    # code run (quietly) before each target
    lapply(list.files("./R", full.names = TRUE), source)
    source("conflicts.R")
    labels <- make_labels()
  },
  names = everything()
)

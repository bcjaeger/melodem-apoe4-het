

# manage packages ---------------------------------------------------------

# these need to be loaded when the pipeline starts
library(targets)
library(tarchetypes)
library(tibble)
library(glue)
library(here)

tar_option_set(
  # these only need to be loaded when targets are being made
  packages = c(
    "tidyverse",  # general data management
    "tidymodels", # general data management
    "data.table", # general data management
    "magrittr",   # pipes!
    "glue",       # string management
    "grf",        # causal random forests
    "aorsf",      # oblique random forests
    "glmnet",     # penalized regression
    "xgboost",    # boosted trees
    "randomForestSRC", # axis based random forests
    "party",           # conditional inference forests
    "riskRegression",  # evaluates prediction accuracy
    "survival"         # provides the Surv() function
  )
)


# Load R functions --------------------------------------------------------

lapply(list.files("./R", full.names = TRUE), source)


# Assertions --------------------------------------------------------------

assert_data_safety()


# Globals -----------------------------------------------------------------

age_range <- c(55, 80)

manuscript_version <- 1

# Individual cohort targets -----------------------------------------------

file_sim_1_tar <- tar_target(
  file_sim_1,
  command = "data/sim_1-raw.csv",
  format = 'file'
)

data_sim_1_tar <- tar_target(
  data_sim_1,
  data_prepare(cohort_name = 'sim_1',
               age_range = age_range)
)

file_sim_2_tar <- tar_target(
  file_sim_2,
  command = "data/sim_2-raw.csv",
  format = 'file'
)

data_sim_2_tar <- tar_target(
  data_sim_2,
  data_prepare(cohort_name = 'sim_2',
               age_range = age_range)
)

# real data cohorts (to be uncommented during exercise)

# file_x_tar <- tar_target(
#   file_x,
#   command = "data/sensitive/x-raw.csv",
#   format = 'file'
# )
#
# data_x_tar <- tar_target(
#   data_x,
#   data_prepare(cohort_name = 'x')
# )

# combining cohorts ----

data_pooled_tar <- tar_target(
  data_pooled,
  data_pool(sim_1 = data_sim_1,
            sim_2 = data_sim_2)
)


# Benchmark targets -------------------------------------------------------

bm_risk_tar <- tar_map(
  values = tibble(subset = list('overall',
                                'sim_1',
                                'sim_2')),
  tar_target(bm_risk, bench_pred_risk(data_pooled,
                                      subset = subset))
)

bm_risk_all_tar <- tar_combine(bm_risk_all, bm_risk_tar$bm_risk)

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
  data_sim_1_tar,
  data_sim_2_tar,
  data_pooled_tar,
  bm_risk_tar,
  bm_risk_all_tar,
  manuscript_tar
)

# hook not needed for tar_make() but is for tar_make_future().
tar_hook_before(
  targets = targets,
  hook = {
    # code run before each target
    lapply(list.files("./R", full.names = TRUE), source)
    source("conflicts.R")
    labels <- make_labels()
  },
  names = everything()
)

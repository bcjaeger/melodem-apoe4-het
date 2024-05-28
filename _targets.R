

# manage packages ---------------------------------------------------------

library(targets)
library(tarchetypes)
library(tibble)
library(glue)
library(here)

tar_option_set(
  packages = c("tidyverse",
               "tidymodels",
               "data.table",
               "magrittr",
               "glue",
               "grf",
               "aorsf",
               "glmnet",
               "xgboost",
               "randomForestSRC",
               "party",
               "riskRegression",
               "survival")
)

# Load R functions
lapply(list.files("./R", full.names = TRUE), source)

# make sure files are not misplaced
assert_data_safety()

age_range <- c(55, 80)

manuscript_version <- 1

if(!dir.exists(glue("manuscript/manuscript-v{manuscript_version}"))){
  dir.create(glue("manuscript/manuscript-v{manuscript_version}"))
}

# individual cohort targets ----

data_sim_1_tar <- tar_target(
  data_sim_1,
  data_prepare(cohort_name = 'sim_1',
               age_range = age_range)
)

# add your cohort here!

data_sim_2_tar <- tar_target(
  data_sim_2,
  data_prepare(cohort_name = 'sim_1',
               age_range = age_range)
)

# combining cohorts ----

data_pooled_tar <- tar_target(
  data_pooled,
  data_pool(sim_1 = data_sim_1,
            sim_2 = data_sim_2)
)

bm_risk_tar <- tar_map(
  values = tibble(subset = list('overall',
                                'sim_1',
                                'sim_2')),
  tar_target(bm_risk, bench_pred_risk(data_pooled,
                                      subset = subset))
)

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


targets <- list(
  data_sim_1_tar,
  data_sim_2_tar,
  data_pooled_tar,
  bm_risk_tar,
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

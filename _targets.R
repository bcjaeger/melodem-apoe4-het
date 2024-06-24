

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

# these don't need to be made on new systems
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

fit_orsf_sim_tar <- tar_target(
  fit_orsf_sim,
  fit_orsf_clsf(data = data_sim)
)

fit_grf_sim_tar <- tar_target(
  fit_grf_sim,
  fit_grf_surv(data = data_sim,
               fit_orsf = fit_orsf_sim)
)

# real data model targets (to be added as an exercise)


# Shar-eable targets ------------------------------------------------------

orsf_shareable_sim_tar <- tar_target(
  orsf_shareable_sim,
  orsf_summarize(fit_orsf_sim)
)

grf_shareable_sim_tar <- tar_target(
  grf_shareable_sim,
  grf_summarize(fit_grf_sim)
)

# uncomment and run line below to save shareables
# write_shareables(.names = c("orsf_shareable_sim", "grf_shareable_sim"))


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
  # penguin_figs_tar,
  file_sim_tar,
  data_sim_tar,
  fit_orsf_sim_tar,
  fit_grf_sim_tar,
  orsf_shareable_sim_tar,
  grf_shareable_sim_tar
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

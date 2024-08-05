

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

labels <- make_labels()

# slides targets ----------------------------------------------------------

# these don't need to be made for now
# penguin_figs_tar <- tar_target(penguin_figs, viz_penguins())

# Data management targets -------------------------------------------------

# run usethis::edit_r_environ() to access and edit .Renviron variables

# run targets::tar_invalidate(names = c('file', 'data_melodem')) to
# make targets manually re-make the file and data targets.

file_tar <- tar_target(
  file,
  command = Sys.getenv("melodem_data_fpath"),
  format = 'file'
)

data_melodem_tar <- tar_target(
  data_melodem,
  data_prepare(file,
               labels,
               trt_var = Sys.getenv("melodem_data_trt_var"),
               time_var = Sys.getenv("melodem_data_time_var"),
               status_var = Sys.getenv("melodem_data_status_var"),
               age_range = c(55, 80))
)

horizon_grid_tar <- tar_target(
  horizon_grid,
  create_horizon_grid(data_melodem)
)

# Model targets -----------------------------------------------------------

fit_orsf_tar <- tar_target(
  fit_orsf,
  fit_orsf_clsf(data = data_melodem,
                select_variables = FALSE)
)

fit_grf_tar <- tar_target(
  fit_grf,
  fit_grf_surv(data = data_melodem,
               trt_random = Sys.getenv("melodem_trt_random"),
               fit_orsf = fit_orsf,
               horizon = horizon_grid),
  iteration = 'list',
  pattern = map(horizon_grid)
)


# Shar-eable targets ------------------------------------------------------

orsf_shareable_tar <- tar_target(
  orsf_shareable,
  orsf_summarize(fit_orsf)
)

grf_shareable_tar <- tar_target(
  grf_shareable,
  map_dfr(set_names(fit_grf, horizon_grid),
          ~grf_summarize(.x, vars = c("age",
                                      "sex_female")),
          .id = 'horizon')
)

# uncomment and run line below to save shareables
# write_shareables(.names = c("orsf_shareable", "grf_shareable"))


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
  file_tar,
  data_melodem_tar,
  horizon_grid_tar,
  fit_orsf_tar,
  fit_grf_tar,
  orsf_shareable_tar,
  grf_shareable_tar
)


# hook not needed for tar_make() but is for tar_make_future().
tar_hook_before(
  targets = targets,
  hook = {
    # code run (quietly) before each target
    lapply(list.files("./R", full.names = TRUE), source)
    source("conflicts.R")
  },
  names = everything()
)

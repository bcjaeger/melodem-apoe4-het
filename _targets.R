

# manage packages ---------------------------------------------------------

source("_targets_packages.R")
source("_targets_conflicts.R")

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

time_vars_tar <- tar_target(
  time_vars,
  command = {

    time_vars <- c("time", "age")

    time_vars_missing <- setdiff(time_vars, names(data_melodem$values))

    if(!is_empty(time_vars_missing)){
      stop("Missing time variable:", time_vars_missing)
    }

    time_vars

  }

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
               time_var = time_vars,
               horizon = horizon_grid),
  pattern = cross(horizon_grid, time_vars)
)


# Shar-eable targets ------------------------------------------------------

orsf_shareable_tar <- tar_target(
  orsf_shareable,
  orsf_summarize(fit_orsf)
)

cate_shareable_tar <- tar_target(
  cate_shareable,
  command = {
    mutate(fit_grf, cate = map(fit, ~ get_scores(.x)), .keep = 'unused')
  }
)

grf_shareable_tar <- tar_target(
  grf_shareable,
  command = {

    fit_grf %>%
      mutate(
        summary = map2(
          .x = fit,
          .y = time_var,
          .f = ~ .x %>%
            grf_summarize(vars = setdiff(c("age", "sex_female"), .y))
        )
      ) %>%
      select(-fit) %>%
      unnest(summary)

  }

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
  time_vars_tar,
  fit_orsf_tar,
  fit_grf_tar,
  orsf_shareable_tar,
  cate_shareable_tar,
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

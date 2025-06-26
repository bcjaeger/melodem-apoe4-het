
# agenda:
# - gifts for data stewards
# - tour of newest code
# - demo - how to use the labels
# - renv, set up each steward
# - run results / discuss


# manage packages ---------------------------------------------------------

source("_targets_packages.R")
source("_targets_conflicts.R")

# Load R functions --------------------------------------------------------

lapply(list.files("./R", full.names = TRUE), source)


# Assertions --------------------------------------------------------------

assert_data_safety()

# Globals -----------------------------------------------------------------

manuscript_version <- 1

trt_random <- 'yes'

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

horizon_grid_time_tar <- tar_target(
  horizon_grid_time,
  create_horizon_grid_time(data_melodem)
)

horizon_grid_age_tar <- tar_target(
  horizon_grid_age,
  create_horizon_grid_age(data_melodem)
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
                labels = labels,
                select_variables = FALSE)
)

fit_coxph_tar <- tar_target(
  fit_coxph,
  command = {

    names_X <- names(labels$variables) %>%
      unique() %>%
      intersect(names(data_melodem$values))

    ctns_X <- names_X %>%
      intersect(names(select(data_melodem$values, where(is.numeric))))

    formula <- paste("Surv(time, status)",
                     paste(names_X, collapse = " + "),
                     sep = " ~ ") %>%
      as.formula()

    formula_split <- update(formula, .~. -treatment)

    data_model <- data_melodem$values %>%
      select(time, status, all_of(names_X)) %>%
      mutate(across(all_of(ctns_X), ~ .x / sd(.x, na.rm = TRUE)))

    data_model_imputed <-
      mice(data_model, m = 5, method = 'rf') %>%
      complete(action = 'all')

    fit_overall <- data_model_imputed %>%
      map(~ coxph(formula = formula, data = .x)) %>%
      pool()

    fit_by_apoe <- data_model_imputed %>%
      map(~ split(.x, .x$treatment)) %>%
      map(~ list(carrier = coxph(formula = formula_split,
                                 data = .x$carrier),
                 non_carrier = coxph(formula = formula_split,
                                     data = .x$non_carrier)))

    fit_carrier <- pool(map(fit_by_apoe, "carrier"))
    fit_non_carrier <- pool(map(fit_by_apoe, "non_carrier"))

    list(
      overall = fit_overall,
      non_carrier = fit_non_carrier,
      carrier = fit_carrier
    )

  }
)

fit_grf_time_tar <- tar_target(
  fit_grf_time,
  fit_grf_surv(data = data_melodem,
               labels = labels,
               trt_random = trt_random,
               fit_orsf = fit_orsf,
               time_var = 'time',
               horizon = horizon_grid_time),
  pattern = map(horizon_grid_time)
)

fit_grf_age_tar <- tar_target(
  fit_grf_age,
  fit_grf_surv(data = data_melodem,
               labels = labels,
               trt_random = trt_random,
               fit_orsf = fit_orsf,
               time_var = 'age',
               horizon = horizon_grid_age),
  pattern = map(horizon_grid_age)
)

fit_grf_tar <- tar_target(fit_grf, bind_rows(fit_grf_time, fit_grf_age))

# Shar-eable targets ------------------------------------------------------

characteristics_shareable_tar <- tar_target(
  characteristics_shareable,
  characteristics_summarize(data_melodem, labels = labels)
)

incidence_shareable_tar <- tar_target(
  incidence_shareable,
  incidence_summarize(data_melodem)
)

orsf_shareable_tar <- tar_target(
  orsf_shareable,
  orsf_summarize(fit_orsf)
)

coxph_shareable_tar <- tar_target(
  coxph_shareable,
  command = {
    map_dfr(fit_coxph,
            tidy, conf.int = TRUE, exponentiate = TRUE,
            .id = 'group') %>%
      as_tibble() %>%
      select(group, term, estimate, p.value, conf.low, conf.high)
  }
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
          .f = ~  .x %>%
              grf_summarize(vars = colnames(.x$X.orig))
        )
      ) %>%
      select(-fit) %>%
      unnest(summary)

  }

)

# # uncomment and run line below to save shareables
# write_shareables(.names = c("incidence_shareable",
#                             "characteristics_shareable",
#                             "orsf_shareable",
#                             "coxph_shareable",
#                             "grf_shareable"))


# Manuscript targets ------------------------------------------------------

tbl_characteristics_tar <- tar_target(
  name = tbl_characteristics,
  command = {

    shareable_subdirs <- list.dirs('shareable/')[-1]

    if(is_empty(shareable_subdirs)) return(NULL)

    names(shareable_subdirs) <- shareable_subdirs %>%
      str_extract("_.*$") %>%
      str_remove("^_")

    tbl_data <- map_dfr(
      shareable_subdirs,
      .id = 'study',
      .f = ~ {

        input <- .x %>%
          file.path('characteristics_shareable.rds') %>%
          read_rds() %>%
          set_names(str_remove_all(names(.), "\\*"))

        sample_sizes <- enframe(names(input),
                                name = 'value',
                                value = 'name') %>%
          mutate(value = str_extract(name, "N = .*$"),
                 value = str_remove(value, "^N = ")) %>%
          pivot_wider(names_from = name, values_from = value) %>%
          mutate(Characteristic = "No. of participants")

        bind_rows(sample_sizes, input) %>%
          rename_with(.fn = ~str_extract(.x, "^\\w+"))

      }
    )

    tbl_data %>%
      as_grouped_data(groups = 'study') %>%
      as_flextable(hide_grouplabel = TRUE) %>%
      flextable_polish() %>%
      flextable_autofit() %>%
      set_header_labels(
        non_carrier = "Non-carrier",
        carrier = "Carrier"
      ) %>%
      add_header_row(
        values = c("Characteristic", "Overall", "Apolipoprotein E"),
        colwidths = c(1, 1, 2)
      ) %>%
      merge_v(part = 'header') %>%
      padding(padding.left = 15,
              j = ~ Characteristic,
              i = ~ Characteristic %in% c("male", "female",
                                          "White", "Black", "Asian", "Other"),
              part = 'body')

  }
)

fig_incidence_tar <- tar_target(
  name = fig_incidence,
  command = {

    shareable_subdirs <- list.dirs('shareable/')[-1]

    if(is_empty(shareable_subdirs)) return(NULL)

    names(shareable_subdirs) <- shareable_subdirs %>%
      str_extract("_.*$") %>%
      str_remove("^_")

    fig_data <- map_dfr(
      shareable_subdirs,
      .id = 'study',
      .f = ~ {
        .x %>%
          file.path('incidence_shareable.rds') %>%
          read_rds()
      }
    )

    ggplot(fig_data) +
      aes(x = time, y = est, color = treatment) +
      geom_line() +
      facet_wrap(~study) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            text = element_text(size = 16)) +
      scale_y_continuous(labels = scales::percent) +
      labs(color = labels$variables['treatment'],
           x = "Time, years",
           y = "Cumulative incidence, %")

  }
)

tbl_coxph_tar <- tar_target(
  name = tbl_coxph,
  command = {

    shareable_subdirs <- list.dirs('shareable/')[-1]

    if(is_empty(shareable_subdirs)) return(NULL)

    names(shareable_subdirs) <- shareable_subdirs %>%
      str_extract("_.*$") %>%
      str_remove("^_")

    tbl_data <- map_dfr(
      shareable_subdirs,
      .id = 'study',
      .f = ~ {
        fpath <- .x %>%
          file.path('coxph_shareable.rds')

        if(!file.exists(fpath)) return(NULL)

        read_rds(fpath)
      }
    ) %>%
      select(-p.value) %>%
      mutate(
        tbl_val = table_glue("{estimate}\n({conf.low}, {conf.high})"),
        .keep = 'unused'
      ) %>%
      pivot_wider(names_from = group, values_from = tbl_val) %>%
      mutate(term = recode(term,
                           "sexfemale" = "Women versus men",
                           "treatmentcarrier" = "Apolipoprotein E",
                           "age" = "Age"))

    as_grouped_data(tbl_data, groups = 'study') %>%
      as_flextable(hide_grouplabel = TRUE) %>%
      flextable_polish() %>%
      flextable_autofit() %>%
      set_header_labels(
        term = "Characteristic",
        overall = "Overall",
        non_carrier = "Non-carrier",
        carrier = "Carrier"
      ) %>%
      add_header_row(
        values = c("Characteristic", "Hazard ratio (95% CI)"),
        colwidths = c(1, 3)
      ) %>%
      merge_v(part = 'header')


  }
)

tbl_het_tar <- tar_target(
  name = tbl_het,
  tidy_eval = FALSE,
  command = {

    shareable_subdirs <- list.dirs('shareable/')[-1]

    if(is_empty(shareable_subdirs)) return(NULL)

    names(shareable_subdirs) <- shareable_subdirs %>%
      str_extract("_.*$") %>%
      str_remove("^_")

    tbls <- map_dfr(
      shareable_subdirs,
      .id = 'study',
      .f = ~ {
        .x %>%
          file.path('grf_shareable.rds') %>%
          read_rds()
      }
    ) %>%
      select(-rate_apriori) %>%
      unnest(blp) %>%
      filter(blp_term != "(Intercept)") %>%
      mutate(blp_term = str_replace(blp_term, "raceeth", "race"),
             blp_term = recode(blp_term,
                               "race_unknown" = "race_Other",
                               "race_Hisp" = "race_Hispanic")) %>%
      split(.$time_var) %>%
      map(
        ~ {

          .time_var <- .x$time_var[1]

          tbl_data_format <- .x %>%
            mutate(ate = table_glue("{ate_est}\n({ate_lwr}, {ate_upr})"),
                   rate_pv = table_pvalue(rate_pval, drop_leading_zero = FALSE),
                   blp = table_glue("{blp_est}\n({blp_lwr}, {blp_upr})"),
                   horizon = table_value(horizon),
                   .keep = 'unused') %>%
            select(-blp_pval) %>%
            filter(str_detect(blp_term, "^age|^race|^sex")) %>%
            filter(!str_detect(blp_term, "unknown$")) %>%
            filter(blp_term != .time_var) %>%
            pivot_wider(names_from = blp_term, values_from = blp)

          horizon_lab <- switch(.time_var,
                                'age' = "Age, years",
                                'time' = "Time since baseline visit, years")

          lookup_vec <- c(
            "Women versus men" = "sex_female",
            "Men versus women" = "sex_male",
            "Age, per year" = "age",
            "Black versus White" = "race_Black",  # JW will come back
            "White versus black" = "race_White",
            "Other versus White" = "race_Other",
            "Asian versus White" = "race_Asian",
            "Hispanic versus White" = "race_Hispanic"
          )

          lookup_vec <- lookup_vec[lookup_vec %in% names(tbl_data_format)]

          ncol_blp <- ncol(tbl_data_format) - 5

          tbl_data_format %>%
            select(-time_var) %>%
            rename(!!!lookup_vec) %>%
            as_grouped_data(groups = 'study') %>%
            as_flextable(hide_grouplabel = TRUE) %>%
            set_header_labels(
              horizon = horizon_lab,
              ate = "Estimated effect of APOE-4 on dementia-free survival time, years",
              rate_pv = "P-value for heterogeneity in effect"
            ) %>%
            add_header_row(
              values = c(
                horizon_lab,
                "Estimated effect of APOE-4 on dementia-free survival time, years",
                "P-value for heterogeneity in effect",
                "Best linear projection of effect"
              ),
              colwidths = c(1,1,1, ncol_blp)
            ) %>%
            merge_v(part = 'header') %>%
            flextable_polish() %>%
            flextable_autofit()
        }

      )
  }
)

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
  horizon_grid_time_tar,
  horizon_grid_age_tar,
  time_vars_tar,
  fit_orsf_tar,
  fit_coxph_tar,
  fit_grf_time_tar,
  fit_grf_age_tar,
  fit_grf_tar,
  characteristics_shareable_tar,
  incidence_shareable_tar,
  orsf_shareable_tar,
  coxph_shareable_tar,
  cate_shareable_tar,
  grf_shareable_tar,
  tbl_characteristics_tar,
  fig_incidence_tar,
  tbl_het_tar,
  tbl_coxph_tar,
  manuscript_tar
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

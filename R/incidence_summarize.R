
incidence_summarize <- function(data_melodem){

  require(cmprsk)

  trt <- Sys.getenv("melodem_data_trt_var")

  cuminc <- cuminc(ftime = data_melodem$values$time,
                   fstatus = data_melodem$values$status,
                   group = data_melodem$values[[trt]])

  bind_rows(no = as_tibble(cuminc[[1]]),
            yes = as_tibble(cuminc[[2]]),
            .id = 'temp') %>%
    rename_with(.fn = function(x) trt, .cols = temp)

}

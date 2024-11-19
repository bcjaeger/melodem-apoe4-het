incidence_summarize <- function(data_melodem){

  require(cmprsk)

  cuminc <- cuminc(ftime = data_melodem$values$time,
                   fstatus = data_melodem$values$status,
                   group = data_melodem$values$treatment)

  bind_rows(no = as_tibble(cuminc[[1]]),
            yes = as_tibble(cuminc[[2]]),
            .id = 'treatment')

}

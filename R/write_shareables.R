

last <- function(x) x[length(x)]

write_shareables <- function(.names){

  stopifnot(is.character(.names))

  targets::tar_load(names = all_of(.names))

  for(i in .names){

    if(!("shareable" %in% stringr::str_split(i, '_')[[1]])){
      stop("target '", i, "' is not shareable.", call. = FALSE)
    }

    readr::write_rds(eval(rlang::parse_expr(i)),
                     file = file.path("shareable", paste0(i, '.rds')))

  }

}

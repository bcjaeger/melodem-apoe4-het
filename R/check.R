
#' @description
#' catch data with names that don't match the data set reqs
#'

check_names <- function(data, expected_names){

  missing_names <- setdiff(expected_names, names(data))

  if(!is_empty(missing_names)){
    warning("the following names were not found: ",
            glue_collapse(missing_names,
                          sep = ", ",
                          last = " and "), ".",
            " Make sure these variables are included in your data.",
            " If needed, rename them when you write your data's",
            " data_select() function")
  }

}

check_fctr_levels <- function(x, name, expected_levels){

  if(!is.factor(x)){
    x <- as.factor(x)
  }

  x_levels <- levels(x)

  missing_levels <- setdiff(expected_levels, x_levels)

  if(!is_empty(missing_levels)){
    stop("missing levels in ", name, ": ",
         glue::glue_collapse(missing_levels, sep = ', ', last = ' and '),
         call. = FALSE)
  }

  extra_levels <- setdiff(x_levels, expected_levels)

  if(!is_empty(extra_levels)){
    warning("extra levels in ", name, ": ",
         glue::glue_collapse(extra_levels, sep = ', ', last = ' and '),
         call. = FALSE)
  }

}

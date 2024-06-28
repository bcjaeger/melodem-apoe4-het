
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

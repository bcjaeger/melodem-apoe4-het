#' @description
#' protect your data by asserting conditions that ensure it isn't committed
#' to be pushed to github.
#'
#' @author bcjaeger
assert_data_safety <- function() {

  if(!dir.exists('data/sensitive')){
    dir.create('data/sensitive/')
  }

  files_expected = c("sim-raw.csv")

  files_in_data <- setdiff(list.files('data'), "sensitive")

  if(!is_equivalent(files_in_data, files_expected)){
    stop("detected misplaced files in the data directory: ",
         glue::glue_collapse(
           setdiff(files_in_data, c("sensitive", "sim-raw.csv")),
           sep = ", ", last = ', and '
         ),
         ". All data files should go into data/sensitive, because files",
         " in this directory are not pushed to GitHub. The 'sim-raw.csv'",
         " file is the only data file intended to be sent to GitHub"
    )
  }

}





#' @description
#' helper functions to organize abbreviations in table footnotes
#'

abbrvs_paste <- function(strings){

  front <- 'Abbreviations'

  matter <- glue::glue_collapse(strings,
                                sep = '; ',
                                last = '; and ')

  paste(front, matter, sep = ': ')

}

abbrvs_write <- function(abbr){

  sorted <- abbr[sort(names(abbr))]

  strings <- map2_chr(names(sorted),
                      sorted,
                      paste, sep = ' = ')

  as_paragraph(abbrvs_paste(strings))

}

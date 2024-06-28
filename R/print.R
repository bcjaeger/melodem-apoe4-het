
#' generic print function for melodem data objects.

print.melodem_data <- function(x, ...){

  header <- " melodem workshop data values "

  if(!is.null(attr(x, 'label'))){
    header <- attr(x, 'label')
  }

  ndash <- 0.5*(cli::console_width() - nchar(header))
  decoration <- paste(rep("-", ndash), collapse = '')
  cat(decoration, header, decoration, '\n')


  print(tibble::as_tibble(x$values), n = 5)

  ndash <- 0.5*(cli::console_width() - nchar(" exclusions "))
  decoration <- paste(rep("-", ndash), collapse = '')
  cat("\n", decoration, " exclusions ", decoration, '\n')
  print(x$exclusions)


}

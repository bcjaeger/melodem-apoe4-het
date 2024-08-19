
uses_lifecourse <- function(x){
  stopifnot(inherits(x, 'melodem_data'))
  attr(x, 'lifecourse')
}

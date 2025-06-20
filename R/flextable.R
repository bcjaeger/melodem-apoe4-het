

flextable_polish <- function(ft,
                             labels,
                             header_text = NULL,
                             footnotes = NULL) {

  if(!is.null(header_text)){ ft %<>% add_header_lines(header_text) }
  if(!is.null(footnotes))  { ft %<>% add_footer_lines(footnotes)   }

  ft %>%
    bold(part = 'header') %>%
    theme_box() %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "all")

}

flextable_autofit <- function(ft,
                              prop_used_col_1 = NULL,
                              width_max = 7){

  n_cols <- ncol(ft$header$dataset)

  stopifnot(n_cols > 1)

  width_1 <- width_max * (prop_used_col_1 %||% (1 / n_cols))

  width_other <- (width_max - width_1) / (n_cols-1)


  ft %>%
    width(width = width_other) %>%
    width(j = 1, width = width_1)

}

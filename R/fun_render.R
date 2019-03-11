#' Render an dt object in latex format
#' @param x A dt object returned by the \code{create_dt()} function
#' @param title A title to the table.
#' @param font_size A numeric value to set the font size.
#' @examples
#' data <- ggplot2::mpg %>%
#'    select(manufacturer, displ, cyl, cty, hwy) %>%
#'    filter(manufacturer %in% c("audi", "chevrolet", "dodge", "ford")) %>%
#'
#' data %>% epitable(group = cyl) %>%
#'    epitable_to_latex(title = "Compare characteristics of manufacturer", save = T)


dt_to_latex <- function(x, title = NULL, font_size = NULL){

  align = print_align(x)
  indent = which(!x$table$variable %in% x$options$var_names$name)
  header = print_header(x)

  # use kable and kableExtra
  tbl <- x$table %>%
    dplyr::mutate_all(~replace_na(.x, "")) %>%
    knitr::kable(format = "latex",
          booktab = T,
          align = align,
          col.names = NULL,
          caption = title) %>%
    kableExtra::kable_styling(font_size = font_size) %>%
    kableExtra::add_indent(indent) %>%
    kableExtra::add_header_above(header)

  return(tbl)
}

#' Render an epitable x in html format
#' @param x An epitable x returned by the \code{epitable()} function
#' @param title A character string to add a title to the table.
#' @param font_size A numeric value to set the font size.
#' @examples
#' data <- ggplot2::mpg %>%
#'    select(manufacturer, displ, cyl, cty, hwy) %>%
#'    filter(manufacturer %in% c("audi", "chevrolet", "dodge", "ford"))
#'
#' data %>% epitable(group = cyl) %>%
#'    epitable_to_html(title = "Compare characteristics of manufacturer", save = T)

dt_to_html <- function(x, title = NULL, font_size = NULL){

  align = print_align(x)
  indent = which(!x$table$variable %in% x$options$var_names$name)
  header = print_header(x)

  # use knitr and kableExtra to make the table
  tbl <- x$table %>%
    dplyr::mutate_all(~replace_na(.x, "")) %>%
    knitr::kable(format = "html",
          booktab = T,
          align = align,
          caption = title,
          escape = F) %>%
    kableExtra::kable_styling(bootstrap_options = "hover",
                  font_size = font_size,
                  full_width = F) %>%
    kableExtra::add_indent(indent) %>%
    kableExtra::add_header_above(header)

  # transform kable as xml to use xml2 package
  tbl <- kableExtra::kable_as_xml(tbl)
  # remove original header with xml2 package
  tbl %>%
    xml2::xml_child("thead") %>%
    xml2::xml_child(2) %>%
    xml2::xml_remove()
  # change color bottom-line header
  tbl %>%
    xml2::xml_child("thead") %>%
    xml2::xml_child() %>% xml2::xml_children() %>% xml2::xml_children() %>%
    xml2::xml_set_attr("style", stringr::str_replace(xml2::xml_attr(.,"style"), "#ddd", "black"))
  # add top horizontal line
  tbl %>%
    xml2::xml_child("thead") %>% xml2::xml_child() %>%
    xml2::xml_set_attr("style", "border-top:1px solid black")
  # add bottom horizontal line
  tbl %>%
    xml2::xml_child("tbody") %>% xml2::xml_child(nrow(x$table)) %>%
    xml2::xml_set_attr("style", "border-bottom:1px solid black")

  # transform xml to kable
  tbl <- kableExtra::xml_as_kable(tbl)

  return(tbl)
}

#' Render an epitable x in a word document
#' @param x An epitable x returned by the \code{epitable()} function
#' @param title A character string to add a title to the table.
#' @param font_size A numeric value to set the font size.
#' @examples
#' data <- ggplot2::mpg %>%
#'    select(manufacturer, displ, cyl, cty, hwy) %>%
#'    filter(manufacturer %in% c("audi", "chevrolet", "dodge", "ford"))
#'
#' data %>% epitable(group = cyl) %>%
#'    epitable_to_word(title = "Compare characteristics of manufacturer")

dt_to_flextable <- function(x, title = NULL, font_size = NULL){

  # vector of alignement
  align = print_align(x)
  # select rows to indent
  indent = which(!x$table$variable %in% x$options$var_names$name)
  header_df <- tibble::tibble(col_keys = names(x$table),
                      name = rep(names(print_header(x)), print_header(x)))

  # make a flextable
  ft <- flextable::flextable(x$table) %>%
    flextable::border_remove() %>%
    flextable::set_header_df(header_df) %>%
    # merge identical group names in header
    flextable::merge_h(part = "header") %>%
    # add horizonral line like booktabs theme
    flextable::hline_bottom(border = officer::fp_border(), part = "all") %>%
    flextable::hline_top(border = officer::fp_border(), part = "header") %>%
    # fontbold header
    flextable::bold(part = "header") %>%
    # indent values of categorical variables and font italic
    flextable::padding(i = indent, j = 1, padding.left = 25) %>%
    flextable::italic(i = indent, j = 1)


  # set header alignment
  ft <- purrr::reduce2(
    .x = 1:ncol(x$table),
    .y = print_header_align(x),
    .f = function(value, arg1, arg2){flextable::align(value, j = arg1, align = arg2, part = "header")},
    .init = ft
  )

  # set body alignment
  ft <- purrr::reduce2(
    .x = 1:ncol(x$table),
    .y = case_when(
      align == "r" ~ "right", align == "l" ~ "left",
      TRUE ~ as.character(align)
    ),
    # function to set the right alignment to the right column
    .f = function(value, arg1, arg2){flextable::align(value, j = arg1, align = arg2)},
    .init = ft
  )

  # adjust width
  ft <- flextable::autofit(ft)

  return(ft)
  }



#' Render an epitable object in html format
#' @param object An epitable object returned by the \code{epitable()} function
#' @param title A character string to add a title to the table.
#' @param font_size A numeric value to set the font size.
#' @param save A T/F logical. If true it will save the table within a html page
#' in the working directories.
#' @examples
#' data <- ggplot2::mpg %>%
#'    select(manufacturer, displ, cyl, cty, hwy) %>%
#'    filter(manufacturer %in% c("audi", "chevrolet", "dodge", "ford"))
#'
#' data %>% epitable(group = cyl) %>%
#'    epitable_to_html(title = "Compare characteristics of manufacturer", save = T)

epitable_to_html <- function(object, title = NULL, font_size = NULL, save = F){

  align = print_align(object)
  indent = which(!object$table$variable %in% object$options$var.names$name)
  header = print_header(object)

  # use knitr and kableExtra to make the table
  tbl <- object$table %>%
    mutate_all(~replace_na(.x, "")) %>%
    kable(format = "html",
          booktab = T,
          align = align,
          caption = title,
          escape = F) %>%
    kable_styling(bootstrap_options = "hover",
                  font_size = font_size,
                  full_width = F) %>%
    add_indent(indent) %>%
    add_header_above(header)

  # transform kable as xml to use xml2 package
  tbl <- kable_as_xml(tbl)
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
    xml2::xml_child("tbody") %>% xml2::xml_child(nrow(object$table)) %>%
    xml2::xml_set_attr("style", "border-bottom:1px solid black")

  # transform xml to kable
  tbl <- xml_as_kable(tbl)

  # return
  if(save){
    tbl %>% save_kable("epitable.html")
  } else {
    return(tbl)
  }
}

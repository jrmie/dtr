#' Render an epitable object in latex format
#' @param object An epitable object returned by the \code{epitable()} function
#' @param title A character string to add a title to the table.
#' @param font_size A numeric value to set the font size.
#' @param save A T/F logical. If true it will save the table within a pdf file
#' in the working directorie.
#' @examples
#' data <- ggplot2::mpg %>%
#'    select(manufacturer, displ, cyl, cty, hwy) %>%
#'    filter(manufacturer %in% c("audi", "chevrolet", "dodge", "ford")) %>%
#'
#' data %>% epitable(group = cyl) %>%
#'    epitable_to_latex(title = "Compare characteristics of manufacturer", save = T)


epitable_to_latex <- function(object, title = NULL, font_size = NULL, save = F){

  align = print_align(object)
  indent = which(!object$table$variable %in% object$options$var.names$name)
  header = print_header(object)

  # use kable and kableExtra
  tbl <- object$table %>%
    mutate_all(~replace_na(.x, "")) %>%
    kable(format = "latex",
          booktab = T,
          align = align,
          col.names = NULL,
          caption = title) %>%
    kable_styling(font_size = font_size) %>%
    add_indent(indent) %>%
    add_header_above(header)

  # return
  if(save){
    tbl %>% save_kable("epitable.pdf")
  } else {
    return(tbl)
  }
}

#' Render an epitable object in a word document
#' @param object An epitable object returned by the \code{epitable()} function
#' @param title A character string to add a title to the table.
#' @param font_size A numeric value to set the font size.
#' @examples
#' data <- ggplot2::mpg %>%
#'    select(manufacturer, displ, cyl, cty, hwy) %>%
#'    filter(manufacturer %in% c("audi", "chevrolet", "dodge", "ford"))
#'
#' data %>% epitable(group = cyl) %>%
#'    epitable_to_word(title = "Compare characteristics of manufacturer")

epitable_to_word <- function(object, title = NULL, font_size = NULL, save = F){

  align = print_align(object)
  indent = which(!object$table$variable %in% object$options$var.names$name)
  header = print_header(object)

  # make a flextable
  ft <- flextable::flextable(object$table)

  # set the alignment
  ft <- reduce2(
    # argument 1 to select column
    .x = 1:ncol(object$table),
    # argument 2 to set alignment
    .y = case_when(
      align == "r" ~ "right", align == "l" ~ "left",
      TRUE ~ as.character(align)
    ),
    # function to set the right alignment to the right column
    .f = function(value, arg1, arg2){align(value, j = arg1, align = arg2)},
    # use an initial value for ft
    .init = ft
  )

  }


# Set the alignement to the header
print_header_align <- function(x){
  align <- c(1, 2:(length(x$options$grp_levels)+1))
  names(align) <- c("left", rep("center", length(x$options$grp_levels)))
  if(x$options$compare){
    align <- c(align, "right" = length(align)+1)
  }
  return(align)
}

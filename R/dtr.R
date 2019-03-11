#' dtr: A package for produce descriptive tables.
#'
#' @docType package
#' @name dtr
#'
#' @importFrom magrittr %>%
#' @import tidyr
#' @import kableExtra
#' @import rlang
#' @export dt_create
#' @export dt_to_html
#' @export dt_to_latex
#' @export dt_to_flextable
#' @export dt_save
NULL
#' Create an dt object
#'
#' @param data A data frame which contains only factor or numeric variables.
#' @param group Characters. A grouping variable name.
#' @param stat_num Characters. Specify which statistics should be provide.
#' @param digits Integer. Specify the number of digits to display.
#' @param keep_missing A T/F value. Indicate if statistics should be provide for cases with missing grouping variable.
#' @param compare A T/F value. Indicate if a statistical comparison should be added.
#' @param overall A T/F value. Indicate if statistics should be provide for the entire dataset.
#' @param spec_var A list. Named list of the variables for which it specifies name and digits to display.
#' @return An epitable object

dt_create <- function(data,
                      group,
                      stat_num = "median",
                      digits = 2,
                      spec_var = NULL,
                      keep_missing = F,
                      compare = T,
                      overall = F){


  # NSE Tidyeval ------------

  group <- rlang::ensym(group)

  # Check data --------------

  # check classes of variables
  for (i in data){
    if(!is.numeric(i) & !is.factor(i)){
      stop("Variables must be numerics or factors")
    }
  }

  # check argument stat_num
  if (!(stat_num %in% c("median", "mean"))){
    stop("The argument 'stat_num' must be 'median' or 'mean'")
  }


  # Tidy the data -----------

  # handle missing group variable or not
  if (keep_missing){
    data <- data %>%
      mutate(!!group := fct_explicit_na(!!group, na_level = "Missing data"))
  } else {
    data <- data %>%
      filter(!is.na(!!group))
  }

  # groups the data and set a vector of levels
  data <- group_by(data, !!group)
  grp_levels <- levels(data[[group_vars(data)]])

  # set vector of names of numerics and factor variables
  var_num <- data %>% select_if(is.numeric) %>% tbl_nongroup_vars()
  var_fct <- data %>% select_if(is.factor) %>% tbl_nongroup_vars()


  # Describes the data ------

  # summarises numerics and factor
  df_raw <- list(numeric = describe_numeric(data, group, stat_num),
                 factor = describe_factor(data, group))


  # Compare the data --------
  if(compare){
    list_compare <- list(fct = compare_factor(data, group, var_fct),
                         num = compare_numeric(data, group, grp_levels, var_num)
    )
  } else {
    list_compare <- NULL
  }


  # Set how to display ------

  tbl_num <- display_numeric(df_raw$numeric, group, grp_levels, var_num, stat_num, spec_var, digits)
  tbl_fct <- display_factor(df_raw$factor, grp_levels, var_fct)
  tbl <- bind_rows(tbl_num, tbl_fct)

  # add a pvalue column
  if(compare){
    tbl <- display_pvalue(list_compare) %>%
      right_join(tbl, by = "variable")
  }

  # set orders of column and rows
  order <- display_order(data, grp_levels, var_fct, spec_var, compare)
  tbl <- tbl %>%
    select(order$cols) %>%
    arrange(match(variable, order$rows))

  # set var names
  var_names <- display_names(var_num, var_fct, spec_var)
  tbl <- tbl %>%
    mutate(variable = ifelse(variable %in% var_names$variable,
                             var_names$name[match(variable, var_names$variable)],
                             variable)
    )

  # Object returned ---------
  return(list(table = tbl,
              raw_stat = df_raw,
              compare = list_compare,
              options = list(data = data,
                             group = group,
                             grp_levels = grp_levels,
                             stat_num = stat_num,
                             digits = digits,
                             spec_var = spec_var,
                             var_names = var_names,
                             keep_missing = keep_missing,
                             compare = compare,
                             overall = overall)
  )
  )
}


#' Save a rendered dt to pdf, html or docx
#'
#' @param x A table rendered by one of those functions \code{dt_to_html()}, \code{dt_to_latex()} or \code{dt_to_flextable()}
#' @param file A file name without extension specified.
#' @param path A path to directory where file will be save. If NULL file will be save in the working directory.
#' @return A pdf, html or docx file
#'
dt_save <- function(x, file){

  # tidyeval
  file <- rlang::as_string(rlang::ensym(file))

  # save a flextable in a word document
  if (class(x) == "flextable"){
    doc <- officer::read_docx()
    doc <- flextable::body_add_flextable(doc, value = x)
    print(doc, target = paste0(file, ".docx"))

  # save a latex table in pdf document
  } else if(attr(x, "format") == "latex") {
    x %>% kableExtra::save_kable(paste0(file, ".pdf"))

  # save a html table in html page
  } else if(attr(x, "format") == "html") {
    x %>% kableExtra::save_kable(paste0(file, ".html"))
  }
}

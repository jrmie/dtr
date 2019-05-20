#' Create a dt object
#'
#' @param data A data frame which contains only factor or numeric variables.
#' @param group A variable name as characters or symbol which define groups.
#' @param stat_num Characters. Specify the statistic to describe numeric
#'   variables, "median" (default) provides median, first and third quartiles, "mean"
#'   provides mean and standard deviation.
#' @param digits Integer. Specify the number of digits to display.
#' @param keep_missing Logical. If FALSE (default) observations which have a
#'   missing value for the grouping variable are exclude from the dataset.
#'   Otherwise an extra group named "Missing data" will be add to the table to
#'   describe this observations.
#' @param compare Logical. If TRUE (default) a statistical comparison is added
#'   to the table.
#' @param overall Logical. If TRUE an extra column with the statistics of the
#'   entire dataset will be added to the table.
#' @param spec_var List. A named list of the variables for which you want
#'   specify row name, number of digits to display or just ordering the rows.
#'   Each variable-named element need to be a named list (with name and digit
#'   elements) or NULL. If provides here, the specific digit argument overwrite
#'   the general digits argument. The order of the variable-named element define
#'   the order of rows in the table.
#' @examples
#' dt_create(data, group = treatment, stat_num = "mean")
#' @return A dt object (a list)

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
    data <- mutate(data, !!group := fct_explicit_na(!!group, na_level = "Missing data"))
  } else {
    data <- filter(data, !is.na(!!group))
  }

  # groups the data and set a vector of levels
  data <- group_by(data, !!group)
  grp_levels <- levels(pull(data, !!group))

  # set vector names of numerics and factor variables
  var_num <- data %>% select_if(is.numeric) %>% tbl_nongroup_vars()
  var_fct <- data %>% select_if(is.factor) %>% tbl_nongroup_vars()


  # Describes the data ------

  # summarises numerics and factor
  df_raw <- list(numeric = describe_numeric(data, var_num, stat_num),
                 factor = describe_factor(data, var_fct))


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

#'Save to .pdf, .html or .docx
#'
#'\code{dt_save} export a rendered dt object in the corresponding file format
#'
#'@param x A dt object rendered by one of those functions \code{dt_to_html()},
#'  \code{dt_to_latex()} or \code{dt_to_flextable()}
#'@param file A file name without extension file specified.
#'@param path A path to directory where file should be save. If NULL (default)
#'  file will be save in the current working directory.
#'@exemples
#'dt_create(data, group = treatment, stat_num = "mean") %>%
#'  dt_to_latex(title = "Compare treated and untreated individuals") %>%
#'  dt_save(file = "table_treatment_groups")
#'@return pdf, html or docx file

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

#' Create an epitable object
#'
#' @param data A data frame which contains only factor or numeric variables.
#' @param group Characters. A grouping variable name.
#' @param stat.num Characters. Specify which statistics should be provide.
#' @param digits Integer. Specify the number of digits to display.
#' @param keep.missing A T/F value. Indicate if statistics should be provide for cases with missing grouping variable.
#' @param compare A T/F value. Indicate if a statistical comparison should be added.
#' @param overall A T/F value. Indicate if statistics should be provide for the entire dataset.
#' @param spec.var A list. Named list of the variables for which it specifies name and digits to display.
#' @return An epitable object

epitable <- function(data,
                     group,
                     stat.num = "median",
                     digits = 2,
                     spec.var = NULL,
                     keep.missing = F,
                     compare = T,
                     overall = F){


  # Check data --------------

  # check classes of variables
  for (i in data){
    if(!is.numeric(i) & !is.factor(i)){
      stop("Variables must be numerics or factors")
    }
  }

  # check argument group
  if (!is.character(group)){
    stop("The argument 'group' must be a character name variable")
  }

  # check argument stat.num
  if (!(stat.num %in% c("median", "mean"))){
    stop("The argument 'stat.num' must be 'median' or 'mean'")
  }


  # Tidy the data -----------

  # use a NSE tidyeval
  group <- sym(group)

  # handle missing group variable or not
  if (keep.missing){
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
  var.num <- data %>% select_if(is.numeric) %>% tbl_nongroup_vars()
  var.fct <- data %>% select_if(is.factor) %>% tbl_nongroup_vars()


  # Describes the data ------

  # summarises numerics and factor
  df_raw <- list(numeric = describe_numeric(data, group, stat.num),
                 factor = describe_factor(data, group))


  # Compare the data --------
  if(compare){
    list_compare <- list(fct = compare_factor(data, group, var.fct),
                         num = compare_numeric(data, group, grp_levels, var.num)
                         )
  }


  # Set how to display ------

  tbl_num <- display_numeric(df_raw$numeric, group, grp_levels, var.num, stat.num, spec.var, digits)
  tbl_fct <- display_factor(df_raw$factor, grp_levels, var.fct)
  tbl <- bind_rows(tbl_num, tbl_fct)

  # add a pvalue column
  if(compare){
    tbl <- display_pvalue(list_compare) %>%
      right_join(tbl, by = "variable")
  }

  # set orders of column and rows
  order <- display_order(data, grp_levels, var.fct, spec.var, compare)
  tbl <- tbl %>%
    select(order$cols) %>%
    arrange(match(variable, order$rows))

  # set var names
  var.names <- display_names(var.num, var.fct, spec.var)
  tbl <- tbl %>%
    mutate(variable = ifelse(variable %in% var.names$variable,
                             var.names$name[match(variable, var.names$variable)],
                             variable)
    )

  # Object returned ---------
  return(list(table = tbl,
              raw_stat = df_raw,
              compare = list_compare,
              options = list(data = data,
                             group = group,
                             grp_levels = grp_levels,
                             stat.num = stat.num,
                             digits = digits,
                             spec.var = spec.var,
                             var.names = var.names,
                             keep.missing = keep.missing,
                             compare = compare,
                             overall = overall)
              )
         )
}

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
                      group = NULL,
                      stat_num = "median",
                      digits = 2,
                      spec_var = NULL,
                      keep_missing = F,
                      compare = T,
                      overall = F){

  # Check arguments and NSE ---------------------------------------------------
  data_n <- rlang::ensym(data)
  group <- rlang::enexpr(group)

  # check classes of variables
  for (i in data){
    if(!is.numeric(i) & !is.factor(i)){
      stop(paste0("Variables must be numerics or factors. ", i, " is not."))
    }
  }

  # check stat_num provide is correct
  if (!(stat_num %in% c("median", "mean"))){
    stop("The argument 'stat_num' must be 'median' or 'mean'")
  }

  # Define the grouping variable depending possibilities ----------------------

  if(!is.null(group)){
    if(is.grouped_df(data))
      message(paste0("Grouping by '", group_vars(data),
                     "' has been dropped to be replace by '",
                     as.character(group),"'"))
  } else {
    if(!is.grouped_df(data))
      stop(call. = F,
           paste0("'", as.character(data_n),
                  "' is not a grouped dataframe. Please provide a group argument"))
    if(length(group_vars(data)) > 1)
      warning(call. = F,
              paste0("'", as.character(data_n),
                      "' has more than one grouping variable. '",
                      last(group_vars(data)), "' is used to makes the groups."))
    group <- last(groups(data))
  }

  # Makes a vector of levels for the following functions ----------------------
  grp_levels <- levels(pull(data, !!group))
  if(length(grp_levels) > 5){ # warns if too much levels for readibility
    warning(call. = F,
            "The grouping variable has ", length(grp_levels),
            " levels. I think you should not compare more than 5 groups.")
  }


  # Tidy the data -------------------------------------------------------------

  # handle missing group variable or not
  if (keep_missing){
    data <- mutate(data, !!group := fct_explicit_na(!!group, na_level = "Missing data"))
  } else {
    data <- filter(data, !is.na(!!group))
  }

  # to get grouped data
  data <- group_by(data, !!group)

  # set vector names of numerics and factor variables
  var_num <- select_if(data, is.numeric) %>% tbl_nongroup_vars()
  var_fct <- select_if(data, is.factor) %>% tbl_nongroup_vars()

  # Describes the data --------------------------------------------------------

  # to summarises numerics and factor
  df_raw <- list(numeric = describe_numeric(data, var_num, stat_num),
                 factor = describe_factor(data, var_fct))


  # Compare the data ----------------------------------------------------------

  if(compare){
    list_compare <- list(fct = compare_factor(data, group, var_fct),
                         num = compare_numeric(data, group, grp_levels, var_num))
  } else {
    list_compare <- NULL
  }

  # to set how to display informations ----------------------------------------

  if (!is.null(df_raw$numeric)) {
    tbl_num <- display_numeric(df_raw$numeric, group, grp_levels, var_num, stat_num, spec_var, digits)
  } else {
    tbl_num <- NULL
  }

  if (!is.null(df_raw$factor)) {
    tbl_fct <- display_factor(df_raw$factor, grp_levels, var_fct)
  } else {
    tbl_fct <- NULL
  }

  tbl <- bind_rows(tbl_num, tbl_fct)

  # add a pvalue column
  if(compare){
    tbl <- display_pvalue(list_compare) %>% right_join(tbl, by = "variable")
  }

  # to set orders of column and rows
  order <- display_order(data, grp_levels, var_fct, spec_var, compare)
  tbl <- select(tbl, order$cols) %>% arrange(match(variable, order$rows))

  # to set var names
  var_names <- display_names(var_num, var_fct, spec_var)
  tbl <- mutate(tbl,
                variable = ifelse(variable %in% var_names$variable,
                                  var_names$name[match(variable, var_names$variable)],
                                  variable)
    )

  # Object returned -----------------------------------------------------------
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
                             overall = overall)))
}

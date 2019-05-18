#######################
# Descriptive functions

# to describe numeric variables
describe_numeric <- function(data, var_num, stat_num){

  df <- select(data, var_num, group_cols())
  print(is.grouped_df(df))
  group <- sym(group_vars(data))

  #summarise
  if(stat_num == "median"){
    df <- bind_rows(median = summarise_all(df, median, na.rm = T),
                    q1 = summarise_all(df, quantile, probs = .25, na.rm = T),
                    q3 = summarise_all(df, quantile, probs = .75, na.rm = T),
                    .id = "stat")
  }
  if(stat_num == "mean"){
    df <- bind_rows(mean = summarise_all(df, mean, na.rm = T),
                    sd = summarise_all(df, sd, na.rm = T),
                    .id = "stat")
  }

  #rename
  df <- df %>%
    mutate(!!group := paste0(!!group, "_", stat)) %>%
    select(-stat)

  return(df)
}

# to describe factor variables
describe_factor <- function(data, var_fct){

  df <- select(data, var_fct, group_cols())
  group <- sym(group_vars(data))

  #summarise
  df <- map_dfr(
    tbl_nongroup_vars(df),
    function(x){
      df %>%
        rename(variable = !!sym(x)) %>%
        filter(!is.na(variable)) %>%
        group_by(variable, add = T) %>%
        count() %>%
        spread(!!group, n) %>%
        mutate_at(vars(-group_cols()), ~replace_na(.x, 0)) %>%
        ungroup() %>%
        mutate_if(is.numeric, list(p = ~./sum(., na.rm = T))) %>%
        mutate_at(vars(variable), as.character)
    })

  return(df)
}

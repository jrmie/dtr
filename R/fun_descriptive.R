#######################
# Descriptive functions

# to describe numeric variables
describe_numeric <- function(x, group, stat.num){
  
  #select
  df <- x %>% select_if(is.numeric)
  
  #summarise
  if(stat.num == "median"){
    df <- bind_rows(median = df %>% summarise_all(median, na.rm = T),
                             q1 = df %>% summarise_all(quantile, probs = .25, na.rm = T),
                             q3 = df %>% summarise_all(quantile, probs = .75, na.rm = T),
                             .id = "stat")
  }
  if(stat.num == "mean"){
    df <- bind_rows(mean = df %>% summarise_all(mean, na.rm = T),
                             sd = df %>% summarise_all(sd, na.rm = T),
                             .id = "stat")
  }
  
  #rename
  df <- df %>%
    mutate(!!group := paste0(!!group, "_", stat)) %>%
    select(-stat)
  
  return(df)
}

# to describe factor variables
describe_factor <- function(x, group){
  
  #select
  df <- x %>% select_if(is.factor)
  
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
        mutate_all(~replace_na(.x, 0)) %>%
        ungroup() %>%
        mutate_if(is.numeric, funs(p = ./sum(., na.rm = T)))
    })
  
  return(df)
}

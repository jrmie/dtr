###################
# Display functions


# 1 #
display_numeric <- function(x, group, grp_levels, var_num, stat_num, spec_var, digits){

  # For loop to apply rounding
  for (i in var_num) {
    if(i %in% names(spec_var) && !is.null(spec_var[[i]]$digit)){
      n_digits <- as.numeric(spec_var[[i]]$digit)
    } else {
      n_digits <- digits
    }
    x[i] <- format(round(x[[i]],
                         digits = n_digits),
                   trim = T,
                   nsmall = n_digits)
  }

  # Transpose rows in columns
  x <- x %>%
    gather(variable, value, var_num) %>%
    spread(!!group, value)

  # For loop to set display
  for (i in grp_levels){

      name_stat2 <- sym(paste0(i,  "_stat2"))

      if(stat_num == "median"){
        q1 <- sym(paste0(i, "_q1"))
        q3 <- sym(paste0(i, "_q3"))
        x <- x %>%
          mutate(!!name_stat2 := paste0("[", !!q1, " - ", !!q3, "]")) %>%
          rename_at(paste0(i, "_median"), ~paste0(i, "_stat1")) %>%
          select(-!!q1, -!!q3)
        }

      if (stat_num == "mean"){
        sd <- sym(paste0(i, "_sd"))
        x <- x %>%
          mutate(!!name_stat2 := paste0("(", !!sd, ")")) %>%
          rename_at(paste0(i, "_mean"), ~paste0(i, "_stat1")) %>%
          select(-!!sd)
        }
  }

  x <- x %>% mutate_all(as.character)

  return(x)
}

# 2 #
display_factor <- function(x, grp_levels, var_fct){

  # For loop to set display
  for (i in grp_levels){

    name_stat2 <- sym(paste0(i,  "_stat2"))
    p <- sym(paste0(i, "_p"))

    x <- x %>%
      rename_at(i, ~paste0(i, "_stat1")) %>%
      mutate(!!name_stat2 := paste0("(", format(round(!!p*100, digits = 1), nsmall = 1), "%)")) %>%
      select(-!!p)
  }

  # Add empty group-row  for categorical name
  for (i in var_fct) {
    x <- x %>%
      add_row(variable = i)
  }

  x <- x %>% mutate_all(as.character)

  return(x)
}

# 3 #
display_pvalue <- function(list_compare){

  p_value <- as_tibble(c(list_compare$fct$p_value, list_compare$num$p_value)) %>%
    mutate_all(~ ifelse(.<0.001,
                        "<0.001",
                        format(round(., digits = 3), nsmall = 3))) %>%
    gather("variable", "p-value")

  return(p_value)
}

# 4 #
display_order <- function(data, grp_levels, var_fct, spec_var, compare){

  #columns
   order_cols <- c("variable")
   for (i in grp_levels) {
     order_cols <- c(order_cols, paste0(i, "_stat1"), paste0(i, "_stat2"))
     }
   if(compare){
     order_cols <- c(order_cols, "p-value")
   }

   #rows
   if(!is.null(spec_var)){
     order_rows <- c(names(spec_var), names(data)) %>% unique()
     } else {
       order_rows <- names(data)
       }
   for (i in var_fct) {
     order_rows_a <- order_rows[1:which(order_rows == i)]
     order_rows_b <- order_rows[-(1:which(order_rows == i))]
     order_rows <- c(order_rows_a, levels(data[[i]]), order_rows_b)
   }

   return(list(cols = order_cols, rows = order_rows))
}

# 5 #
display_names <- function(var_num, var_fct, spec_var){

  var_names <- map_dfr(c(var_num, var_fct),
                       ~ list(variable = .x, name = ifelse(is.null(spec_var[[.x]]$name),
                                                           .x,
                                                           spec_var[[.x]]$name)))

  return(var_names)
}

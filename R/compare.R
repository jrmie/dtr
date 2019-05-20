##################
# Compare function

compare_factor <- function(data, group, var_fct){
  # perfoms a Chi test for each factor variable against grouping variable

  list_chi2 <- map(set_names(var_fct),
                   ~ chisq.test(pull(data, !!group), pull(data, .x)))
  list_pvalue <- map(list_chi2, ~.x$p.value)

  obj <- list(test = list_chi2, p_value = list_pvalue)
  class(obj) <- "compare_factor"
  return(obj)
  # return the console test outputs and p-values
}

compare_numeric <- function(data, group, grp_levels, var_num){
  # performs a t-test/ANOVA for each continuous variable against  grouping variable

  # names of variables as symbol
  names(var_num) <- var_num
  var_num <- syms(var_num)

  # 2 groups = student
  if(length(grp_levels) == 2){

    list_test <- map(set_names(var_num),
                     function(x){
                       formula <- expr(!!x ~ !!group)
                       t_test <- t.test(eval(formula),
                                        data,
                                        var.equal = var.test(eval(formula), data)$p.value > 0.05)
                       return(t_test)
                     })
    list_pvalue <- map(list_test, ~.x$p.value)

  # more than 2 groups = ANOVA
  } else {

    list_test <- map(set_names(var_num),
                     function(x){
                       formula <- expr(!!x ~ !!group)
                       aov_test <- aov(eval(formula), data)
                       return(aov_test)
                     })
    list_pvalue <- map(list_test, ~summary(.x)[[c(1, 5, 1)]])
  }

  obj <- list(test = list_test, p_value = list_pvalue)
  class(obj) <- "compare_numeric"
  return(obj)
}

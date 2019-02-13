##################
# Compare function

compare_factor <- function(data, group, var.fct){
  
  names(var.fct) <- var.fct
  list_chi2 <- imap(var.fct,
                    ~ chisq.test(data[[as.character(group)]], data[[.x]])
                    )
  list_pvalue <- imap(list_chi2, ~.x$p.value)
  
  return(list(test = list_chi2, p_value = list_pvalue))
}


compare_numeric <- function(data, group, grp_levels, var.num){
  
  # names of variables as symbol
  names(var.num) <- var.num
  var.num <- syms(var.num)
  
  # 2 groups = student
  if(length(grp_levels) == 2){
    
    list_test <- imap(var.num,
                        function(var = .x, name = .y){
                          
                          formula <- expr(!!var ~ !!group)
                          t_test <- t.test(eval(formula),
                                           data,
                                           var.equal = var.test(eval(formula), data)$p.value > 0.05)
                          
                          return(name = t_test)
                          }
                        )
    
    list_pvalue <- imap(list_test,
                        ~.x$p.value)
    
  # more than 2 groups = ANOVA  
  } else {
      
    list_test <- imap(var.num,
                          function(var = .x, name = .y){
                            
                            formula <- expr(!!var ~ !!group)
                            aov_test <- aov(eval(formula),
                                            data)
                            
                            return(name = aov_test)
                          }
    )
    
    list_summary <- imap(list_test,
                         ~ summary(.x))
    
    list_pvalue <- imap(list_test,
                        ~ summary(.x)[[c(1, 5, 1)]]
                        )

  }
  
  return(list(test = list_test,
              summaries = list_summary,
              p_value = list_pvalue))
}


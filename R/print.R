
# Set the alignement of the columns in kable object
print_align <- function(x){
  align <- c("l", rep(c("r", "l"), length(x$options$grp_levels)))
  if(x$options$overall){
    align <- c(align, "r", "l")
  }
  if(x$options$compare){
    align <- c(align, "r")
  }
  return(align)
}

# Set the header to display
print_header <- function(x){
  header <- c(1, rep(2, length(x$options$grp_levels)))
  names(header) <- c("Variable", x$options$grp_levels)
  if(x$options$compare){
    header <- c(header, "p-value" = 1)
  }
  return(header)
}

# Set the alignement to the header
print_header_align <- function(x){
  align <- c("left", rep("center", 2*length(x$options$grp_levels)))
  if(x$options$overall){
    align <- c(align, rep("center", 2))
  }
  if(x$options$compare){
    align <- c(align, "right")
  }
  return(align)
}

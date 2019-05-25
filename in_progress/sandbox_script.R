
test <- ggplot2::mpg %>%
  select(manufacturer, displ, cyl, cty, hwy) %>%
  filter(manufacturer %in% c("audi", "dodge", "ford")) %>%
  #filter(cyl %in% c(4,6)) %>%
  mutate_at(vars(manufacturer, cyl), as.factor)

dt_create(test, cyl)

fun_test(group = manufacturer)
michel <- dt_create(test_2)
pull(groups(test))
last(groups(test))
michel
test_2 <- group_by(test, cyl, add = T)
test <- function(x, y = NULL){
  y <- enexpr(y)
  print(y)
  if(!is.null(y)){
    print(!!y)
  }
}

test("a", michel)

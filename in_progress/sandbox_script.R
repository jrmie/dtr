test <- dplyr::storms %>%
  dplyr::select(year, wind, pressure, category) %>%
  filter(!category == "-1") %>%
  mutate(category = fct_drop(category, "-1"))

test <- ggplot2::mpg %>%
  select(manufacturer, displ, cyl, cty, hwy) %>%
  filter(manufacturer %in% c("audi", "dodge", "ford")) %>%
  #filter(cyl %in% c(4,6)) %>%
  mutate_at(vars(manufacturer, cyl), as.factor)

head(mpg) %>%
  kable(format = "latex", col.names = NULL) %>%
  kable_styling() %>%
  add_header_above(header = c("salut" = 1, "salut" = 1, "salut" = 1,
                              "salut" = 1, "salut" = 1, "salut" = 1,
                              "salut" = 1, "salut" = 1, "salut" = 1,
                              "salut" = 1, "salut" = 1))

t$table
t$raw_stat$numeric
t$compare$fct

t <- test %>%
  dt_create(group = manufacturer, spec_var = list(cyl = list(name = "cylinder"), hwy = list(name = "highwaytohell"))) %>%
  dt_to_flextable()

q <- chisq.test(test$manufacturer, test$cyl)
class(q)
typeof(q)

print(b$compare$fct$test)


#%>%
 # dt_to_flextable()

test %>%
  dt_create(group = "manufacturer")


attr(test2, "format")

print_align(test2)


test2 <- epitable(test, group = "cyl")


test <- epitable_to_word(test2)
epitable_to_html(test2) %>% attr("format")
epitable_to_latex(test2) %>% attr("format")

ft
ft %>% align(j = 1, align = "left")


test$

x <- test2


h_cell <- officer::fp_cell(border.bottom = o_border, margin.left = 1, margin.right = 1)
h_text <-


################
o_border <- officer::fp_border()

header_df <- tibble(col_keys = names(test2$table),
                    name = rep(names(print_header(test2)), print_header(test2)))

align <- print_align(test2)

indent = which(!test2$table$variable %in% test2$options$var.names$name)

ft <- flextable::flextable(test2$table) %>%
  border_remove() %>%
  set_header_df(header_df) %>%
  merge_h(part = "header") %>%
  hline_bottom(border = o_border, part = "all") %>%
  hline_top(border = o_border, part = "header")

ft <- reduce2(
  .x = 1:ncol(test2$table),
  .y = print_header_align(test2),
  .f = function(value, arg1, arg2){align(value, j = arg1, align = arg2, part = "header")},
  .init = ft
)

#align
ft <- reduce2(
  # argument 1 to select column
  .x = 1:ncol(test2$table),
  # argument 2 to set alignment
  .y = case_when(
      align == "r" ~ "right", align == "l" ~ "left",
      TRUE ~ as.character(align)
    ),
  # function to set the right alignment to the right column
  .f = function(value, arg1, arg2){align(value, j = arg1, align = arg2, part = "body")},
  .init = ft
     )

# auto width
ft <- autofit(ft)

# bold header
ft <- flextable::bold(ft, part = "header")

# indent
ft %>% flextable::padding(i = indent, j = 1, padding.left = 25) %>%
  italic(i = indent, j = 1)

################


doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "test.docx")

test %>% group_by(cyl) %>% groups() %>% levels(test$.)


fun_test <- function(group){
  group <- ensym(group)
  qq_show(!!group)
}


fun_test(group = manufacturer)

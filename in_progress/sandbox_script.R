test <- dplyr::storms %>%
  dplyr::select(year, wind, pressure, category) %>%
  filter(!category == "-1") %>%
  mutate(category = fct_drop(category, "-1"))

test <- mpg %>%
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


print_align(test2)


test2 <- epitable(test, group = "cyl")


align <- print_align(test2)
ft
ft %>% align(j = 1, align = "left")

header_df <- tibble(col_keys = names(test2$table),
                    name = rep(names(print_header(test2)), print_header(test2)))
ft <- flextable::flextable(test2$table) %>%
  set_header_df(header_df) %>%
  merge_h(part = "header")

ft <- reduce2(
  .x = print_header_align(test2),
  .y = names(print_header_align(test2)),
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

ft %>% set_header_df(tibble(col_keys = names(test2$table), name = rep(names(print_header(test2)),
                                                               print_header(test2)))) %>%
  merge_h(part = "header")

ft %>% set_header_labels(values = set_names(rep(names(print_header(test2)),
                                                print_header(test2)),
                                            names(test2$table))) %>%
  merge_h(i = 1, part = "header")

set_head

flextable::head
print_header(test2) %>% names()
ft %>% merge_at(i = 2:3, j = 1, part = "header")
set_names(rep(names(print_header(test2)),
              print_header(test2)),
          names(test2$table))

print_header_align(test2)

case
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "test.docx")




fun_test <- function(data, ...){
  vars <- ensyms(...)
  data %>% select(!!!vars)
}

test %>% fun_test(hwy, "manufacturer", cyl)

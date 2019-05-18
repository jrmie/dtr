context("Display settings")

load("tests.Rdata")
row_names <- display_names(c("v1", "v2"), c("f1", "f2"), list(v1 = list(name = "variable1"), f1 = list(name = "factor1")))
orders <- display_order(group_by(data_test, g1), c("a", "b"), c("f1", "f2"), list(f1 = NULL, v2 = NULL), T)
pvalue <- display_pvalue(list(fct = compare_factor(data_test, sym("g1"), c("f1", "f2")),
                              num = compare_numeric(data_test, sym("g1"), c("a", "b"), c("v1", "v2"))))
num_median <- display_numeric(describe_numeric(group_by(data_test, g1), c("v1", "v2"), "median"),
                            sym("g1"), c("a", "b"), c("v1", "v2"), "median", list(v2 = list(digits = 4)), 2)
num_mean <- display_numeric(describe_numeric(group_by(data_test, g1), c("v1", "v2"), "mean"),
                            sym("g1"), c("a", "b"), c("v1", "v2"), "mean", list(v2 = list(digits = 4)), 2)
fct <- display_factor(describe_factor(group_by(data_test, g1), c("f1", "f2")), c("a", "b"), c("f1", "f2"))


test_that("Row names are correct", {
  expect_s3_class(row_names, "tbl_df")
  expect_equal(dim(row_names), c(4, 2))
  expect_equal(row_names[[c(1,1)]], "v1")
  expect_equal(row_names[[c(2,3)]], "factor1")
  expect_type(row_names$variable, "character")
  expect_type(row_names$name, "character")
})

test_that("Row and column orders are correct", {
  expect_type(orders, "list")
  expect_length(orders, 2)
  expect_equal(orders$cols, c("variable", "a_stat1", "a_stat2", "b_stat1", "b_stat2", "p-value"))
  expect_equal(orders$rows, c("f1", "a", "b", "c",  "v2", "v1", "f2", "d",  "e",  "f",  "g1", "g2"))
})

test_that("P-values are correctly provides", {
  expect_s3_class(pvalue, "tbl_df")
  expect_equal(dim(pvalue), c(4, 2))
  expect_equal(pvalue$variable, c("f1", "f2", "v1", "v2"))
  expect_equal(pvalue$`p-value`, c("0.970", "0.970", "0.083", "<0.001"))
})

test_that("Numerical variables are correctly display with mean", {
  expect_s3_class(num_mean, "tbl_df")
  expect_equal(dim(num_mean), c(2, 5))
  expect_equal(names(num_mean), c("variable", "a_stat1", "b_stat1", "a_stat2", "b_stat2"))
  expect_equal(num_mean[[c(2,1)]], "0.11")
  expect_equal(num_mean[[c(5,2)]], "(5.5540)")
})

test_that("Numerical variables are correctly display with median", {
  expect_s3_class(num_median, "tbl_df")
  expect_equal(dim(num_median), c(2, 5))
  expect_equal(names(num_median), c("variable", "a_stat1", "b_stat1", "a_stat2", "b_stat2"))
  expect_equal(num_median[[c(2,1)]], "0.27")
  expect_equal(num_median[[c(5,2)]], "[15.8787 - 23.2500]")
})

test_that("Categorical variables are correctly display", {
  expect_s3_class(fct, "tbl_df")
  expect_equal(dim(fct), c(8, 5))
  expect_equal(names(fct), c("variable", "a_stat1", "b_stat1", "a_stat2", "b_stat2"))
  expect_equal(fct$variable, c("a", "b", "c", "d", "e", "f", "f1", "f2"))
  expect_equal(fct[[c(2,1)]], "17")
  expect_equal(fct[[c(4,5)]], "(32.0%)")
  expect_true(is.na(fct[[c(5,8)]]))
})

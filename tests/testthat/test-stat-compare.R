context("Variable comparisons between groups")

load("tests.RData")
obj_g <- compare_factor(data = data_test, group = sym("g1"), var_fct = c("f1", "f2"))
obj_g1 <- compare_numeric(data = data_test, group = sym("g1"), grp_levels = c("a", "b"), var_num = c("v1", "v2"))
obj_g2 <- compare_numeric(data = data_test, group = sym("g2"), grp_levels = c("a", "b", "c", "d"), var_num = c("v1", "v2"))

test_that("test that compare_factor() with return a 2 elements list with chi2 tests and p-values", {
  expect_s3_class(obj_g, "compare_factor")
  expect_type(obj_g, "list")
  expect_length(obj_g, 2)
  expect_s3_class(obj_g$test$f1, "htest")
  expect_s3_class(obj_g$test$f2, "htest")
  expect_type(obj_g$p_value$f1, "double")
  expect_type(obj_g$p_value$f2, "double")
})

test_that("test that compare_factor() with group[2] return a 2 elements list with t-test tests and p-values", {
  expect_s3_class(obj_g1, "compare_numeric")
  expect_type(obj_g1, "list")
  expect_length(obj_g1, 2)
  expect_s3_class(obj_g1$test$v1, "htest")
  expect_s3_class(obj_g1$test$v2, "htest")
  expect_type(obj_g1$p_value$v1, "double")
  expect_type(obj_g1$p_value$v2, "double")
})

test_that("test that compare_factor() with group[4] return a 2 elements list with aov tests and p-values", {
  expect_s3_class(obj_g2, "compare_numeric")
  expect_type(obj_g2, "list")
  expect_length(obj_g2, 2)
  expect_s3_class(obj_g2$test$v1, c("aov", "lm"))
  expect_s3_class(obj_g2$test$v2, c("aov", "lm"))
  expect_type(obj_g2$p_value$v1, "double")
  expect_type(obj_g2$p_value$v2, "double")
})

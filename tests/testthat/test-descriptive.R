context("Numerical descriptions")

load("tests.Rdata")
num_mean <- describe_numeric(group_by(data_test, g2), c("v1", "v2"), "mean")
num_median <- describe_numeric(group_by(data_test, g2), c("v1", "v2"), "median")
fct <- describe_factor(group_by(data_test, g2), c("f1", "f2"))

test_that("mean decription of numeric variables provides the expected tibble", {
  expect_equal(as.data.frame(num_mean), as.data.frame(true_num_mean))
})

test_that("median decription of numeric variables provides the expected tibble", {
  expect_identical(as.data.frame(num_median), as.data.frame(true_num_median))
})

test_that("percent decription of factor variables provides the expected tibble", {
  expect_identical(as.data.frame(fct), as.data.frame(true_factor))
})

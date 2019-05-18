library(testthat)
library(dtr)

test_check("dtr")

# data_test <- data.frame(v1 = as.numeric(rnorm(100)), # numeric no diff
#                         v2 = as.numeric(rnorm(100, mean = c(10, 20), sd = c(2.5, 5))), # numeric diff
#                         f1 = as.factor(c(rep(letters[1], 33), rep(letters[2], 33), rep(letters[3], 34))), # factor no diff
#                         f2 = as.factor(c(rep(letters[4:6], 33), "d")), # factor diff
#                         g1 = as.factor(rep(letters[1:2], 50)), # group 2 cat
#                         g2 = as.factor(rep(letters[1:4], 25))) # group 4 cat

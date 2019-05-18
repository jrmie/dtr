context("Render to formats .html, .pdf and .docx")

load("tests.Rdata")
html <- dt_to_html(dt_create(data_test, g1))
latex <- dt_to_latex(dt_create(data_test, g1))
flextbl <- dt_to_flextable(dt_create(data_test, g1))

test_that("it gets a html format", {expect_equal(attr(html, "format"), "html")})
test_that("it gets a latex format", {expect_equal(attr(latex, "format"), "latex")})
test_that("it gets an html format", {expect_equal(attr(flextbl, "class"), "flextable")})

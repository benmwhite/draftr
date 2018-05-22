context("get_porp()")
library(draftr)

test_that("Output format is correct", {
  #whole output
  expect_output(str(get_porp(bball)), "List of 2")
  #df
  expect_is(get_porp(bball)$df, "data.frame")
  #chart
  expect_is(get_porp(bball)$chart, c("gg", "ggplot"))
  #same number of players in both input and output
  expect_equal(nrow(bball), nrow(get_porp(bball)$df))
})

test_that("Input error handling is correct", {
  #non data frame input
  expect_error(get_porp(3))
  #appropriate variable names missing
  expect_error(get_porp(unname(bball)))
  #remaining columns that are numeric projections
  expect_error(get_porp(cbind(bball, "I am not a fantasy projection")))
})

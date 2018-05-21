context("get_porp()")
library(draftr)

test_that("Output format is correct", {
  expect_output(str(get_porp(bball)), "List of 2")
  expect_is(get_porp(bball)$df, "data.frame")
  expect_is(get_porp(bball)$chart, c("gg", "ggplot"))
  expect_equal(nrow(bball), nrow(get_porp(bball)$df))
})

test_that("output is a plot and number of unique pitch types is greater than 0", {
  expect_type(location_heatmap(deGrom), "list")
})

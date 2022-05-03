test_that("output is a plot and number of unique pitch types is greater than 0", {
  expect_type(location_heatmap(deGrom), "list")
  expect_type(location_heatmap(file = deGrom, by_pitch_type = FALSE), "list")
})

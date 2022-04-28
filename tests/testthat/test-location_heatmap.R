test_that("output is a plot and number of unique pitch types is greater than 0", {
  expect_type(location_heatmap(deGrom), "list")
})


test_that("pitch type greater than 0", {
  expect_true(length(unique(deGrom$pitch_type)) > 0)
})

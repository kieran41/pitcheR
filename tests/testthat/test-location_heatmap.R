file = deGrom

test_that("output is a list", {
  expect_type(location_heatmap(file), "list")
})

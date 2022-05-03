test_that("output is a plot", {
  expect_type(contact_location_heatmap(file = deGrom), "list")
  expect_type(contact_location_heatmap(file = deGrom, by_pitch_type = FALSE), "list")
  expect_type(contact_location_heatmap(file = deGrom, hit.status = TRUE), "list")
  expect_type(contact_location_heatmap(file = deGrom, by_pitch_type = FALSE, hit.status = TRUE), "list")
})


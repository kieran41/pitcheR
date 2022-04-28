file = deGrom

test_that("velocity is numeric", {
  expect_type(file$release_speed, "double")
})

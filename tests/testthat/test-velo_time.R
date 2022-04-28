file = deGrom

test_that("output is a plot", {
  expect_type(velo_bp(file), "list")
})


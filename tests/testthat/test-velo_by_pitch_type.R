file = deGrom

test_that("velocity is numeric", {
  expect_type(velo_bp(file), "list")
})

file = deGrom

test_that("Filtering of pitch type is working as expected",{
  require("dplyr")
  expect_true(length(unique(file[,"pitch_type"])) == 4)
}, )

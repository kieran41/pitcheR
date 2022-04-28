test_that("Filtering of pitch type is working as expected",{
  #require("dplyr")
  expect_true(length(unique(deGrom[,"pitch_type"])) == 4)
})

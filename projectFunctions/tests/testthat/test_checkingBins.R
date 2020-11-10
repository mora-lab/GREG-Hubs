context("Bin Intervals") 

test_that("Test that the input is a valid entity", {expect_identical(checkingBins("LULU"), "Invalid cell-type.") })
test_that("Test that all criteria are met and the data is well structured", {expect_identical(checkingBins("A549"), "All good!") })
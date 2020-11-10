context("Counts per feature") 

test_that("Test for a valid input for cell-type", {expect_identical(checkingDataMatrixFormat("LULU"), "Invalid cell-type.") })
test_that("Test that the data is well structured", {expect_identical(checkingDataMatrixFormat("A549"), "All good!") })
test_that("Test that the download is available", {expect_identical(checkingDataMatrixFormat("MCF7"), "Ouch. Did you miss something!") })
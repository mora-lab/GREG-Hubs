context("Checking download status of BAM files") 

test_that("Test that an argument is not missing", {expect_match(downloadBAMfiles("LULU"), "Argument missing.") })
test_that("Test that argument(s) are not missing", {expect_match(downloadBAMfiles(), "Argument missing.") })
test_that("Test that argument(s) are not supplied as a list", {expect_match(downloadBAMfiles(c("A549,CTCF")), "Argument missing.") })
test_that("Test that the input cell-type of feature is invalid", {expect_match(downloadBAMfiles("LULU", "YY1"), "Invalid cell-type or feature.") })
test_that("Test the integrity of the order of arguments", {expect_match(downloadBAMfiles("YY1", "A549"), "Invalid cell-type or feature.") })
test_that("Test that the download is successful and file saved", {expect_match(downloadBAMfiles("A549", "CTCF"), "Files successfully downloaded and saved.") })

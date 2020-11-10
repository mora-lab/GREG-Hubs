context("Coverages from BAM files") 

test_that("Test that the download is available", {expect_silent(postProcessBAMFiles("A549")) })
test_that("Test that an error is returned for an invalid cell-type as input", {expect_identical(postProcessBAMFiles("LULU"), "Invalid cell-type.") })

context("Reading LR hubs from GREG") 

test_that("Some LULU has no recognition as a valid cell-type", {expect_match(consolidateLRRegions("LULU"), "Invalid cell-type.") })
test_that("Expect that output is a data-frame", {expect_s3_class(consolidateLRRegions("A549"), "data.frame") })
test_that("Expect three columns in total", {expect_equal(ncol(consolidateLRRegions("A549")), 3)})

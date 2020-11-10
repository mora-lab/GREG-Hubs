context("Reading Hubs") 

test_that("Expect that output is a list", {expect_s3_class(readingHubs(), "list") })
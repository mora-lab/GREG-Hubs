context("BPM Normalization")

test_that("Expect that the result is numeric", {expect_is(bpmNormalize(13.0), "numeric") })
test_that("Test that an error is returned for an invalid input", {expect_error(bpmNormalize("NA")) })
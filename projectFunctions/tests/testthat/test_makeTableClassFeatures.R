context("Input data for ML algorithms") 

test_that("Some LULU has no recognition as a valid cell-type", {expect_match(makeTableClassFeatures("LULU"), "Invalid cell-type.") })
test_that("Expect that output is a data-frame", {expect_s3_class(makeTableClassFeatures("A549"), "data.frame") })
test_that("Expect first three columns as chr, start, and end", {expect_identical(colnames(makeTableClassFeatures("A549"))[1:3], c("chr", "start", "end"))})
test_that("Expect last column as Class", {expect_identical(colnames(makeTableClassFeatures("A549"))[ncol(makeTableClassFeatures("A549"))], "Class") })

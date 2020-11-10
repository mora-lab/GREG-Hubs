context("Scaling BED intervals with GREG indices") 


test_that("Unsuccessful execution", {expect_match(prepareMasterIntervalsBinIds("notMe.bed"), "File not found. Please check!") })
# test_that("Successful execution", {expect_match(prepareMasterIntervalsBinIds("hg19_2k_bins.bed"), "File saved successfully!") })
test_that("Expect that output is a data-frame", {expect_s3_class(prepareMasterIntervalsBinIds("hg19_2k_bins.bed"), "data.frame") })
test_that("Expect greater than a row", {expect_gte(nrow(prepareMasterIntervalsBinIds("hg19_2k_bins.bed")), 1)})
test_that("Expect five columns in total", {expect_equal(ncol(prepareMasterIntervalsBinIds("hg19_2k_bins.bed")), 5)})
test_that("Expect column names as chr, start, and end", {expect_identical(colnames(prepareMasterIntervalsBinIds("hg19_2k_bins.bed"))[1:3], c("chr", "start", "end"))})
test_that("Test that second row starts from 2001", {expect_equal(prepareMasterIntervalsBinIds("hg19_2k_bins.bed")[2,2], 2001)})
test_that("Test that second row goes to 4000", {expect_equal(prepareMasterIntervalsBinIds("hg19_2k_bins.bed")[2,3], 4000)})
test_that("Test that tenth row starts from 18001", {expect_equal(prepareMasterIntervalsBinIds("hg19_2k_bins.bed")[10,2], 18001)})
test_that("Test that tenth row goes to 20000", {expect_equal(prepareMasterIntervalsBinIds("hg19_2k_bins.bed")[10,3], 20000)})


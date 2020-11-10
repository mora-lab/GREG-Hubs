context("Performance metrics from a model's evaluation") 

test_that("Test for the execution of the function", {expect_output(moderPerformance( a549, "lr", test, test$Class)) })
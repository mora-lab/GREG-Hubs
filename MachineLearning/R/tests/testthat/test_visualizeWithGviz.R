context("Visualization of the predicted regions (hubs)") 

test_that("Test for the execution of the function", {expect_output(visualizaWithGviz("A549", "A549forML.txt", "chr21", 1, 2, 3, 16, a549PredictionProbs)) })


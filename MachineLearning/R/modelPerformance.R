## Author : Shaurya Jauhari
## Last Reviewed: December 11th, 2020.
## Description: This function takes model, test-data, and test-data class (3 arguments) as input 
## and engenders a comprehensive set of 7 performance metrics, as below:
## (i) Confusion Matrix
## (ii) Sensitivity
## (iii) Specificity
## (iv) ROC-Curve
## (v) Area Under Curve(AUC) of the ROC-Curve
## (vi) Statistical significance for the model
## (vii) Confidence level for the model

## The last two metrics are applicable to linear models only.

modelPerformance <- function (model, modelCategory, testData, testDataClassColumnName) {

## Installing required packages and loading libraries
  
  requiredPackages <- c("ROCR", "caret", "e1071")
  newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
  if(length(newPackages)) install.packages(newPackages, dependencies = TRUE)
   
  suppressPackageStartupMessages(library(e1071))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(library(ROCR))

## Define categories of acceptable models.
## LR: Logistic Regression
## RF: Random Forests

  
  modelCategories <- list("LR", "RF")
  stopifnot(TRUE %in% grepl(modelCategory, modelCategories, ignore.case=TRUE))
  
## Model performance metrics
  
  predictionLabels <- predict(model, testData, type = "response")

## The calculation of prediction probabilities depends on the type of model selected. For logistic regression, the model
## already has them as 'numeric' type, while for random forests, we have to make that conversion from 'factor' to 'numeric'.
  
  ifelse(modelCategory %in% c("LR","lr"), 
         predictionLabelsProbabilities <- ifelse(predictionLabels > 0.5, 1, 0), 
         predictionLabelsProbabilities <- ifelse(as.numeric(predictionLabels)-1 > 0.5, 1, 0))

  confusionMatrix <- table(Predicted = predictionLabelsProbabilities, Actual = eval(parse(text= paste0(deparse(substitute(testData)), 
                                                                                          "$", deparse(substitute(testDataClassColumnName))))))
  cat("The confusion matrix is\n") 
  print(confusionMatrix)
  accuracyClass <- sum(diag(confusionMatrix))/sum(confusionMatrix)
  cat("The accuracy of the model is", accuracyClass*100, "%", "\n")
  misClassError <- 1 - accuracyClass
  cat("The misclassification error of the model is", misClassError*100, "%", "\n")
  
## Create a check for imbalanced data.
  
  if(dim(confusionMatrix)[1] == 1) return("Only a single class predicted. Probably skewed data handling.")
  
  cat("The sensitivity of the model is", (sensitivity(confusionMatrix))*100, "%", "\n")
  cat("The specificity of the model is", (specificity(confusionMatrix))*100, "%", "\n")

## ROC curve

  predictionResults <- prediction(predictionLabelsProbabilities, eval(parse(text= paste0(deparse(substitute(testData)),
                                                                            "$", deparse(substitute(testDataClassColumnName))))))
  performanceResults <- performance(predictionResults, "tpr", "fpr")
  plot(performanceResults, main = "ROC Curve", colorize = TRUE)
  abline(a = 0, b = 1)
  
## AUC
  
  aucFind <- performance(predictionResults, measure = "auc")
  aucFind <- aucFind@y.values[[1]]
  cat("The area under curve is", aucFind, "\n")

## Statistical Significance of the model (linear models only; logistic regression is a linear model)
  
  if(modelCategory %in% c("LR","lr"))
  {
    overallP <- with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    cat("The statistical significance for the model is", overallP, "\n")
    cat("The confidence level for the model is", ((1-overallP)*100), "%")
  }
    
  else
  {
    cat("\n")
  }
    
}

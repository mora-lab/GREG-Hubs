## Author : Shaurya Jauhari
## Last Reviewed: October 15th, 2020.
## Description: This function takes a feature-file, a chromosome-name, and a feature-name as arguments and 
## adds random values between the maximum value and twice the maximum value of the read-coverages of that 
## feature. This distorts proportion of that feature to the overall classification model and might be useful
## for cross-validation.


dataSimulation <- function(dataFile, chrName, featureName){
  
  ## Check for file availability
  
  if(file.exists(dataFile))
  {
    standardData <- read.table(dataFile, header = TRUE)
  }
  else
  {
    return("Invalid file.")
  }
  
  ## Extracting chromosome-specific data
  
  stopifnot(chrName %in% unique(standardData$chr)) ## check for a valid chromosome
  chrData <- standardData[standardData$chr == chrName,]
  
  ## Locating feature data to be tampered
  
  stopifnot(featureName %in% colnames(chrData)) ## check for a valid column-name
  selectedFeature <- chrData[ , featureName] 
  
  
  ### SIMULATION ###
  ## The idea is to add exorbitant values to the base values to the selected feature (read-coverage)
  ## that is the most influential variable to the model. For the same, we plan on adding random values
  ## between the original maximum value of the coverage and twice that number.
  
  ## Generate random numbers in the mentioned range
  selectedFeature <- runif(length(selectedFeature), min = max(selectedFeature), max = 2 * max(selectedFeature))
  
  chrData[ , featureName] <- selectedFeature
  return(chrData)
}
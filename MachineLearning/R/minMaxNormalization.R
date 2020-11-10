## Author : Shaurya Jauhari
## Last Reviewed: October 10th, 2020.
## Description: This function takes a data frame as an argument and applies the Min-Max Normalization
## on the columns that hold numeric values. This is extensively useful in the deep learning models that
## warrant such format of data.


minMaxNormalization <- function(inputData){
  for(i in 1:length(inputData))
  {
    if(is.numeric(inputData[[i]]) | is.double(inputData[[i]]))
    {
      inputData[[i]] <- (inputData[[i]] - min(inputData[[i]], na.rm = T))/(max(inputData[[i]], na.rm = T) - 
                                                                             min(inputData[[i]], na.rm = T))
    }
  }
  return(inputData)
}
checkingDataMatrixFormat <- function(cell){
  
  cells <- c("A549", "H1ESC", "HELA", "IMR90", "K562", "MCF7")
  
  if(cell %in% cells)
  {
    ## Load coverage data file
    covFile <- read.table(paste0(getwd(), "/GREG/", cell, "/normalizedReads.txt"), header = TRUE)
    
    ## Column count
    numberColumns <- ncol(covFile)
    
    ## Number of folders for features
    numberFolders <- length(list.dirs(path = paste0(getwd(), "/GREG/", cell, "/")))
    
    ## Condition checking | Have all features been covered in columns, appropriate rows exist (i.e. greater than 1),
    ## and no NA value exists in the data (i.e. just one factor level : FALSE)?
    ifelse(numberColumns==numberFolders & nrow(covFile)>1 & length(levels(factor(is.na.data.frame(covFile))))==1, 
           return("All good!"), return("Ouch! Did you miss something?"))
  }
  
  else
  {
    return("Invalid cell-type.")
  }

}
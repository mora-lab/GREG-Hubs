## Author : Shaurya Jauhari
## Last Reviewed: September 15th, 2020.
## Description: This function assembles together the data-matrix(coverage scores for all 
## features), corresponding intervals(bins), and the LR-hubs for each cell-type, and outputs
## a compound data-matrix for machine learning application.

makeTableClassFeatures <- function(cell){
  cells <- c("A549", "H1ESC", "HELA", "IMR90", "K562", "MCF7")
  
  if(cell %in% cells)
  {

  scoreTable <- read.table(paste0(getwd(),"/GREG/",cell, "/normalizedReads.txt"), header = TRUE) ## importing scores
  
  LRhubs <- read.table(paste0(getwd(),"/GREG/",cell, "/",cell, "LRs.txt"), header = TRUE) ## class:hub
  bins <- read.table(paste0(getwd(),"/GREG/",cell, "/binsRegions.txt"), header = TRUE) ## all regions
  colnames(bins) <- c("chr", "start", "end") ## renaming columns
  
  ## combining regions to scores
  
  masterTable <- cbind(bins, scoreTable)
  
  ## Hubs
  
  library(dplyr)
  hubs <- inner_join(masterTable, LRhubs)
  hubs$Class <- "Hub"
  
  
  ## Non-hubs
  
  nonHubs <- anti_join(masterTable, LRhubs)
  nonHubs$Class <- "Non-Hub"
  
  ## Combining data for both classes into a super-set.
  
  megaTable <- full_join(hubs, nonHubs)
  megaTable <- megaTable[with(megaTable, order(chr, start)), ] ## sorting the data
  
  ## Saving Results
  
  write.table(megaTable, paste0(getwd(),"/MachineLearning/", cell, "forML.txt"),
              sep = "\t",
              row.names = FALSE,
              quote = FALSE)
  
  return(megaTable)
  }
  
  else
  {
    return("Invalid cell-type.")
  }
  
}

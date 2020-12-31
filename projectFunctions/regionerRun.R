## Author : Shaurya Jauhari
## Last Reviewed: December 31st, 2020.
## Description: This function generates regioneR plots for the feature-specific long range interactions
## and global long-range interactions. These plots signify not only the statistical significance of the overlap,
## but also engender a profile of how each of the marks distribute around the hubs. 
## Additionally, the output is saved as an external file with appropriate nomenclature.  


regionerRun <- function(featureFilePath, backgroundRegionsPath, cellName, featureName)
{
  # --- Install and Load package library --- # 
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  
  requiredPackages <- "regioneR"
  newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
  if(length(newPackages)) BiocManager::install(newPackages)
  suppressPackageStartupMessages(library(regioneR))
  
  
  # --- Source Files --- #
  if(file.exists(backgroundRegionsPath)){
    
    # --- background file --- #
    # a consolidated BED file of all intervals with long range interactions #
    backgroundRegions <- read.table(as.character(backgroundRegionsPath), header = T, sep = " ")
    backgroundRegions <- toGRanges(backgroundRegions) ## converting to a GRanges object.
                                    
  }
  else
  {
    cat ("Background file not found.")
  }
       
  if(file.exists(featureFilePath)){
    
    # --- feature file --- #
    # a consolidated BED file of all feature-specific intervals #
    featureRegions <- read.table(as.character(featureFilePath))
  }
  else
  {
    cat ("Feature file not found.")
  }                 
  
  # --- Running regioneR and saving output plot --- #
  png(file = paste0(as.character(cellName), as.character(featureName), ".png"))
  
  plot(overlapPermTest(A = featureRegions, 
                  B = backgroundRegions, 
                  ntimes = 100, 
                  genome = "hg19", 
                  count.once = TRUE)) ## with coverage values
  dev.off()
}
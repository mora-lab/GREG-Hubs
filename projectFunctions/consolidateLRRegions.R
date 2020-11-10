## Author : Shaurya Jauhari
## Last Reviewed: September 15th, 2020.
## Description: This function assembles together the defined LR regions into a single,
## contiguous range and saves to a file, exclusive to each cell type. This function
## takes the name of the cell-type as input.

consolidateLRRegions <- function(cell){

  cells <- c("A549", "H1ESC", "HELA", "IMR90", "K562", "MCF7")
  
  if(cell %in% cells)
  {
    # Extracting BED files from the input directory; searches for sub-sub folders too.
    
    files <<- list.files(path = paste0(getwd(),"/results/", cell, "/LR-hubs/"), pattern = "\\.bed$") 
    files <<- substr(files,1,nchar(files)-4) # Clipping file extension to retrieve sample names only.
    
    LRregions <- list() ## Creating a consolidated region-set for the long range interactions.
    
    for(i in 1:length(files))
    {
      LRregions[[i]] <- read.table(paste0(getwd(),"/results/", cell, "/LR-hubs/",paste0(eval(parse(text="files[i]")),".bed")), sep = "\t")
      LRregions[[i]] <- LRregions[[i]][,1:3]
      colnames(LRregions[[i]]) <- c("chr", "start", "end")
      LRregions[[i]] <- LRregions[[i]][order(LRregions[[i]]$chr),]
    }
    
    LRregionsAll <- do.call("rbind", LRregions) ## combining all regions under one data-frame
    #write.table(LRregionsAll, file = paste0(getwd(),"/GREG/", cell, "/", cell, "LRs.txt") , row.names = FALSE, col.names = TRUE) ## Save file
    
    return(LRregionsAll)
  }
  
  else
  {
    return("Invalid cell-type.")
  }

} 



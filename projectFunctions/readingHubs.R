## Author : Shaurya Jauhari
## Last Reviewed: September 16th, 2020.
## Filtering Hubs for different cell types.
## The cell-types in questions are all existing cell-types in GREG, except for 
## IPS6.9 and IPS19.11, due to lack of feature data associated with them.

readingHubs <- function(){
  
  ## Loading clustering results from GREG.
  
  load("./data/GREG_cluster_v3.RData")
  
  ## Define valid cell types
  
  cellTypes <- c("IMR90", "MCF7", "HELA", "K562", "H1ESC", "A549")
  validLongRangeRelationships <- Long_Range_relationships[Long_Range_relationships$r_CellType %in% cellTypes, ]
  
  ## From this variable, we shall only be interested in the LR ids( for which we shall
  ## fetching details from another variable), and cell-types. Let's prune the data in 
  ## that accord.
  
  validLongRangeRelationships <- validLongRangeRelationships[,c(1,6)]
  
  ## Next step is to filter LR_ids for each given cell-type as a exclusive data-set.
  
  LRdataCellTypes <- lapply(cellTypes, function(ct) {validLongRangeRelationships[validLongRangeRelationships$r_CellType %in% ct, ]})
  names(LRdataCellTypes) <- cellTypes
  
  
  ## Finding bin ids for given LR ids.
  
  LRids <- names(Bins_genes_per_LR_relationship)
  
  dataLRids <- list()
  
  for (count in 1:length(LRids))
  {
    dataLRids[[count]] <- eval(parse(text = paste0("Bins_genes_per_LR_relationship", 
                                                   "$", LRids[count], "[,1]")))
  }
  
  names(dataLRids) <- LRids
  
  
  ## Let us create third and fourth columns to store bin ids and indices, 
  ## respectively.
  
  
  for (i in 1:length(LRdataCellTypes))
  {
    LRdataCellTypes[[i]][c(3, 4)] <- NA
    colnames(LRdataCellTypes[[i]]) <- c("LR_id", "r_CellType", "binIds", "binIndices")
  }
  
  ## So, we now have the list of lists of bin ids that are associated with the given
  ## LR ids. We shall now map these to the cell types.
  
  
  for (i in 1:length(LRdataCellTypes))
  {
    for (j in 1:nrow(LRdataCellTypes[[i]])) 
    {
      if(LRdataCellTypes[[i]][[1]][[j]] %in% names(dataLRids))
      {
        
        LRdataCellTypes[[i]][[3]][[j]] <- dataLRids[LRdataCellTypes[[i]][[1]][[j]]]
      }
    }
  }
  
  ## For "binIndices" now.
  ## Let us now pull the information of bin indices from the master chart.
  
  binIndices <- read.delim("intervalsMasterReferenceGREG19.txt")
  
  ## Step1 : [k] : Create list
  
  
  for (i in 1:length(LRdataCellTypes))
  {
    for (j in 1:nrow(LRdataCellTypes[[i]])) 
    {
      for(k in 1:length(LRdataCellTypes[[i]][[3]][[j]][[1]]))
      {
        if(LRdataCellTypes[[i]][[3]][[j]][[1]][k] %in% binIndices$binsGREGformat) 
        {
          LRdataCellTypes[[i]][[4]][[j]][k] <- binIndices[binIndices$binsGREGformat 
                                                          == LRdataCellTypes[[i]][[3]][[j]][[1]][k], c(1, 2, 3)]   
        }
      }
    }
  }
  
  
  ## Step2 : [[k]] : Populate list
  
  for (i in 1:length(LRdataCellTypes))
  {
    for (j in 1:nrow(LRdataCellTypes[[i]])) 
    {
      for(k in 1:length(LRdataCellTypes[[i]][[3]][[j]][[1]]))
      {
        if(LRdataCellTypes[[i]][[3]][[j]][[1]][k] %in% binIndices$binsGREGformat) 
        {
          LRdataCellTypes[[i]][[4]][[j]][[k]] <- binIndices[binIndices$binsGREGformat 
                                                            == LRdataCellTypes[[i]][[3]][[j]][[1]][k], c(1, 2, 3)]   
        }
      }
    }
  }
  
  ## Reformatting the bin Indices - list of data-frames to a consolidated data-frame for
  ## all LR_ids.
  
  for (i in 1:length(LRdataCellTypes))
  {
    for (j in 1:nrow(LRdataCellTypes[[i]])) 
    {
      LRdataCellTypes[[i]][[4]][[j]] <- do.call(rbind, LRdataCellTypes[[i]][[4]][[j]])
    }
  }
  
  
  ## Save this LR-hub tabulation as an R object.
  
  save(LRdataCellTypes, file = "LRhubsGREG.Rdata")
  
  
  ## We can remove the cell-names and bin-Ids now, for convenience.
  
  for (i in 1:length(LRdataCellTypes))
  {
    LRdataCellTypes[[i]]["r_CellType"]<- c() # names of cell-lines
    LRdataCellTypes[[i]]["binIds"]<- c() # bin_Ids
  }
  
  
  ## Now that we have the bin indices for all LR-hubs and cell-types, we will be
  ## extracting these regions for both those categories.
  
  ## Structuring directories.
  
  dir.create("./results") ## create a results folder
  
  ## sub-folder for cell specific information 
  
  for (i in 1:length(cellTypes))
  {
    dir.create(paste0("./results/", cellTypes[i]))
  }
  
  cats <- c("LR-hubs", "Bin-hubs")
  
  ## sub-sub-folders for all LR-hubs
  
  for (i in 1:length(cellTypes))
  {
    for(j in 1:length(cats))
    {
      dir.create(paste0("./results/", cellTypes[i], "/", cats[j]))  
    }
  }
  
  ## Saving files to appropriate locations
  
  for(i in 1:length(LRdataCellTypes))
  {
    for(j in 1:nrow(LRdataCellTypes[[i]]))
    {
      write.table(LRdataCellTypes[[i]][[2]][[j]], 
                  file = paste0("./results/", names(LRdataCellTypes)[[i]], 
                                "/LR-hubs/", LRdataCellTypes[[i]][[1]][[j]], ".bed"), 
                  row.names = FALSE, 
                  col.names = FALSE,
                  quote = FALSE,
                  sep = "\t")
    }
  }
  
  return(LRdataCellTypes)
  
}



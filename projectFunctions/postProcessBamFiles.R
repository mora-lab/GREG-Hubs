## Author : Shaurya Jauhari
## Last Reviewed: September 11th, 2020.
# Description: The following function covers "whole 9 yards"!. It takes any cell-type (as defined in "siftedData.xlsx"),
# and calculates the normalized read counts for every feature (again, as defined in "siftedData.xlsx"). The
# file is saved in the home directory of that cell-type.

postProcessBAMFiles <- function(cell)
  {
  cells <- c("A549", "H1ESC", "HELA", "IMR90", "K562", "MCF7")
  
  if(cell %in% cells)
  {
    # Listing of all "readCounts.tab" from each feature.
  
    covFiles <- list.files(path=paste0(getwd(),"/GREG/", cell, "/"), pattern = ".tab", recursive = TRUE)

    # Loading those files to a list of dataframes.
  
    dataCovFiles <- lapply(covFiles, function(path) {
      read.delim( file = paste0(getwd(), "/GREG/", cell, "/", path), header = TRUE, sep = "\t")
      })

     # Naming list indices for intuitive access.

    names(dataCovFiles) <- sub("\\/.*", "", covFiles)

    # Calculating the average of read counts from all replicates present.

    avgReads <- lapply(dataCovFiles, function(avgDf){
      return(cbind(avgDf, rowMeans(avgDf[,-c(1:3)]))) # leaving out the first three columns, i.e. chr, start, end.
    })

    # Trimming the dataframes for relevant columns only.

    selectAvgReads <- lapply(avgReads, function(d){
      subset(d, select = c("X..chr.", "X.start.", "X.end.", "rowMeans(avgDf[, -c(1:3)])"))
    })


    ## Here we diverge.


    # (i) For each cell-type.
    # Merge all dataframes first and then remove the "chr", "start", and "end" columns.

  library(plyr)

  combinedAvgReads <- join_all(selectAvgReads, by=c("X..chr.", "X.start.", "X.end."), type='left')

  # Since we no longer need the bins' attributes, we omit them here. However, we'll save these intervals for later use.
  # Now we're left with the read counts for all features, against each given feature.

  binsRegions <- combinedAvgReads[, c(1:3)]
  binsRegions$X.start. <- binsRegions$X.start. + 1 # 1-Index UCSC format
  
  combinedAvgReads <- combinedAvgReads[, -c(1:3)]
  colnames(combinedAvgReads) <- names(dataCovFiles)

  # Replacing NAs induced by combining dataframes by 0.
  combinedAvgReads[is.na(combinedAvgReads)] <- 0

  # Normalizing reads by BPM.
  normAvgReads <- as.data.frame(sapply(combinedAvgReads, bpmNormalize))

  # Save file and write as output.

  write.table(normAvgReads, file = paste0(getwd(), "/GREG/", cell, "/normalizedReads.txt") , sep = "\t", row.names = FALSE)
  write.table(binsRegions, file = paste0(getwd(), "/GREG/", cell, "/binsRegions.txt") , sep = "\t", row.names = FALSE)


 # (ii) For each feature

   individScores <- selectAvgReads

   for (i in 1:length(individScores))
     {
       # BPM Normalize.
       individScores[[i]]$normReads <- bpmNormalize(individScores[[i]]$`rowMeans(avgDf[, -c(1:3)])`)

       # 1-index the BED file
       individScores[[i]][2] <- individScores[[i]][2] + 1

       # Remove raw read counts
       individScores[[i]][4] <- c()

      # Save and write the respective bedGraph file.

       write.table(individScores[[i]],
                   file = paste0(getwd(), "/GREG/", cell, "/", names(individScores[i]), "/", "normalizedReads.bedGraph"),
                  sep = "\t",
                  quote = FALSE,
                  row.names = FALSE,
                  col.names = FALSE)
    }
  }
  
  else
  {
    return("Invalid cell-type.")
  }
}



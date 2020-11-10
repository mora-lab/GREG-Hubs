checkingBins <- function(cell){
  
  ## Load data
  cells <- c("A549", "H1ESC", "HELA", "IMR90", "K562", "MCF7")
  
  if(cell %in% cells)
  {
    ## Load the intervals' file
    binsFile <- read.table(paste0(getwd(), "/GREG/", cell, "/binsRegions.txt"), header = TRUE)
    
    ## Column count | must be 3: "chr", "start", and "end"
    numberColumns <- ncol(binsFile)
    
    ## Column difference | end - start
    differenceList <- binsFile$X.end. - binsFile$X.start.
    
    
    ## Condition check | consistent column differences except for chromosome-length endings.
    
    if(numberColumns==3 & table(differenceList)[names(table(differenceList)) == 1999] == 
       max(table(differenceList)[names(table(differenceList))]))
      {
          for(chroms in levels(binsFile$X..chr.))
          {
            if(binsFile[binsFile$X..chr.==chroms,][2,]$X.start. ==2001 & binsFile[binsFile$X..chr.==chroms,][10,]$X.start. ==18001)
            {
              return("All good!")
            }
            else
            {
              return("Inconsistent bin intervals.") 
            }
          }
      }
    }
  else
  {
    return("Invalid cell-type.")
  }
}


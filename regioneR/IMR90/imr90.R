files <<- list.files("./results/IMR90/LR-hubs/", pattern = "\\.bed$") # Extracting BED files from the input directory.
files <<- substr(files,1,nchar(files)-4) # Clipping file extension to retrieve sample names only.


LRregions <- list() ## Creating a consolidated region-set for the long range interactions.

for(i in 1:length(files))
{
  LRregions[[i]] <- read.table(paste0("./results/IMR90/LR-hubs/",paste0(eval(parse(text="files[i]")),".bed")), sep = "\t")
  LRregions[[i]] <- LRregions[[i]][,1:3]
  colnames(LRregions[[i]]) <- c("chr", "start", "end")
  LRregions[[i]] <- LRregions[[i]][order(LRregions[[i]]$chr),]
}

LRregionsAll <- do.call("rbind", LRregions) ## combining all regions under one dataframe
write.table(LRregionsAll, file = "./GREG/IMR90/IMR90LRs.txt" , row.names = FALSE, col.names = TRUE) ## Save file

LRregionsAll <- toGRanges(na.omit(LRregionsAll)) ## converting to a GRanges object.


## Epigenome marks for IMR90 cell line

ctcf <- read.table("./GREG/IMR90/CTCF/normalizedReads.bedGraph.bedClipped.sorted")
h3k4me1 <- read.table("./GREG/IMR90/H3K4me1/normalizedReads.bedGraph.bedClipped.sorted")
h3k4me2 <- read.table("./GREG/IMR90/H3K4me2/normalizedReads.bedGraph.bedClipped.sorted")
h3k4me3 <- read.table("./GREG/IMR90/H3K4me3/normalizedReads.bedGraph.bedClipped.sorted")
h3k9ac <- read.table("./GREG/IMR90/H3K9ac/normalizedReads.bedGraph.bedClipped.sorted")
h3k9me3 <- read.table("./GREG/IMR90/H3K9me3/normalizedReads.bedGraph.bedClipped.sorted")
h3k27me3 <- read.table("./GREG/IMR90/H3K27me3/normalizedReads.bedGraph.bedClipped.sorted")
h3k36me3 <- read.table("./GREG/IMR90/H3K36me3/normalizedReads.bedGraph.bedClipped.sorted")
rnapol2 <- read.table("./GREG/IMR90/RNAPol2/normalizedReads.bedGraph.bedClipped.sorted")

## regioneR

## Let us check the number of overlaps in the two regions

numOverlaps(ctcf, LRregionsAll, count.once = TRUE)


lz2 <- localZScore(pt=pt2, A=ctcf, B=toGRanges(result))
plot(lz2)



pt1 <- overlapPermTest(A = ctcf, B = LRregionsAll, 
                       ntimes = 100, count.once = TRUE) ## with coverage values

pt2 <- overlapPermTest(A = h3k4me1, B = LRregionsAll, 
                       ntimes = 100, count.once = TRUE) ## with coverage values

pt3 <- overlapPermTest(A = h3k4me2, B = LRregionsAll, 
                       ntimes = 100, count.once = TRUE) ## with coverage values


pt4 <- overlapPermTest(A = h3k4me3, B = LRregionsAll, 
                      ntimes = 100, count.once = TRUE) ## with coverage values


pt5 <- overlapPermTest(A = h3k9ac, B = LRregionsAll, 
                      ntimes = 100, count.once = TRUE) ## with coverage values


pt6 <- overlapPermTest(A = h3k9me3, B = LRregionsAll, 
                       ntimes = 100, count.once = TRUE) ## with coverage values


pt7 <- overlapPermTest(A = h3k27me3, B = LRregionsAll, 
                       ntimes = 100, count.once = TRUE) ## with coverage values

pt8 <- overlapPermTest(A = h3k36me3, B = LRregionsAll, 
                       ntimes = 100, count.once = TRUE) ## with coverage values

pt9 <- overlapPermTest(A = rnapol2, B = LRregionsAll, 
                       ntimes = 100, count.once = TRUE) ## with coverage values

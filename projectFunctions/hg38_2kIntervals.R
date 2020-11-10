## Creating a background file for hg38 version of the human genome. The intervals are tiled in 2kb regions. 

#BiocManager::install("BSgenome.Hsapiens.UCSC.hg38")
library(BSgenome.Hsapiens.UCSC.hg38)
si <- seqinfo(BSgenome.Hsapiens.UCSC.hg38)
Bins2k <-  tileGenome(si, tilewidth = 2000, cut.last.tile.in.chrom = TRUE)
targetBED <- as.data.frame(Bins2k)
targetBED <- targetBED[, 1:3]
colnames(targetBED) <- c("chrom", "start", "end")
head(targetBED)
write.table(targetBED, file = "../hg38_2k_bins.bed", sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)


files <<- list.files("../results/A549/LR-hubs/", pattern = "\\.bed$") # Extracting BED files from the input directory.
files <<- substr(files,1,nchar(files)-4) # Clipping file extension to retrieve sample names only.


LRregions <- list() ## Creating a consolidated region-set for the long range interactions.

for(i in 1:length(files))
{
  LRregions[[i]] <- read.table(paste0("../results/A549/LR-hubs/",paste0(eval(parse(text="files[i]")),".bed")), sep = "\t")
  LRregions[[i]] <- LRregions[[i]][,1:3]
  colnames(LRregions[[i]]) <- c("chr", "start", "end")
  LRregions[[i]] <- LRregions[[i]][order(LRregions[[i]]$chr),]
}

LRregionsAll <- do.call("rbind", LRregions) ## combining all regions under one dataframe
write.table(LRregionsAll, file = "../GREG/A549/A549LRs.txt" , row.names = FALSE, col.names = TRUE) ## Save file

LRregionsAll <- toGRanges(LRregionsAll) ## converting to a GRanges object.


## Epigenome marks for A549 cell line

ctcf <- read.table("../GREG/A549/CTCF/normalizedReads.bedGraph.bedClipped.sorted")
ep300 <- read.table("../GREG/A549/EP300/normalizedReads.bedGraph.bedClipped.sorted")
h3k4me1 <- read.table("../GREG/A549/H3K4me1/normalizedReads.bedGraph.bedClipped.sorted")
h3k4me2 <- read.table("../GREG/A549/H3K4me2/normalizedReads.bedGraph.bedClipped.sorted")
h3k4me3 <- read.table("../GREG/A549/H3K4me3/normalizedReads.bedGraph.bedClipped.sorted")
h3k9ac <- read.table("../GREG/A549/H3K9ac/normalizedReads.bedGraph.bedClipped.sorted")
h3k9me3 <- read.table("../GREG/A549/H3K9me3/normalizedReads.bedGraph.bedClipped.sorted")
h3k27me3 <- read.table("../GREG/A549/H3K27me3/normalizedReads.bedGraph.bedClipped.sorted")
h3k36me3 <- read.table("../GREG/A549/H3K36me3/normalizedReads.bedGraph.bedClipped.sorted")
rad21 <- read.table("../GREG/A549/RAD21/normalizedReads.bedGraph.bedClipped.sorted")
rnapol2 <- read.table("../GREG/A549/RNAPol2/normalizedReads.bedGraph.bedClipped.sorted")
yy1 <- read.table("../GREG/A549/YY1/normalizedReads.bedGraph.bedClipped.sorted")

## regioneR

## Let us check the number of overlaps in the two regions

numOverlaps(ctcf, LRregionsAll, count.once = TRUE)


lz2 <- localZScore(pt=pt2, A=ctcf, B=toGRanges(result))
plot(lz2)



pt1 <- overlapPermTest(A = ctcf, B = LRregionsAll, 
                       ntimes = 100, genome = "hg19", 
                       count.once = TRUE) ## with coverage values

pt2 <- overlapPermTest(A = ep300, B = LRregionsAll, 
                       ntimes = 100, genome = "hg19", 
                       count.once = TRUE) ## with coverage values

pt3 <- overlapPermTest(A = h3k4me1, B = LRregionsAll, 
                       ntimes = 100, genome = "hg19", 
                       count.once = TRUE) ## with coverage values

pt4 <- overlapPermTest(A = h3k4me2, B = LRregionsAll, 
                       ntimes = 100, genome = "hg19", 
                       count.once = TRUE) ## with coverage values


pt5<- overlapPermTest(A = h3k4me3, B = LRregionsAll, 
                       ntimes = 100, genome = "hg19", 
                       count.once = TRUE) ## with coverage values


pt6<- overlapPermTest(A = h3k9ac, B = LRregionsAll, 
                       ntimes = 100, genome = "hg19", 
                       count.once = TRUE) ## with coverage values


pt7 <- overlapPermTest(A = h3k9me3, B = LRregionsAll, 
                       ntimes = 100, genome = "hg19", 
                       count.once = TRUE) ## with coverage values


pt8 <- overlapPermTest(A = h3k27me3, B = LRregionsAll, 
                       ntimes = 100, genome = "hg19", 
                       count.once = TRUE) ## with coverage values

pt9 <- overlapPermTest(A = h3k36me3, B = LRregionsAll, 
                       ntimes = 100, genome = "hg19", 
                       count.once = TRUE) ## with coverage values


pt10 <- overlapPermTest(A = rad21, B = LRregionsAll, 
                       ntimes = 100, genome = "hg19", 
                       count.once = TRUE) ## with coverage values

pt11<- overlapPermTest(A = rnapol2, B = LRregionsAll, 
                        ntimes = 100, genome = "hg19", 
                        count.once = TRUE) ## with coverage values

pt12 <- overlapPermTest(A = yy1, B = LRregionsAll, 
                       ntimes = 100, genome = "hg19", 
                       count.once = TRUE) ## with coverage values

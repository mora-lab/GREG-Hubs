## BPM (per bin) = number of reads per bin / sum of all reads per bin (in millions). 
## (https://deeptools.readthedocs.io/en/develop/content/tools/bamCoverage.html)

bpmNormalize <- function(reads)
{
  stopifnot(is.numeric(reads)) ## check for appropriate input datatype.
  bpm <- (reads/ (sum(reads)/10^6))
  return(bpm)
}
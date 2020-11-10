#!/bin/bash

# Reading argument values using for loop and processing for samtools sort expression.

for file in "$@"
do
 echo "Processing $file"
 
 ## Extracting file information
## Path
folder=$(dirname "${file}")

## File
BAMfile=`basename $file .bam`

## Execute samtools sort command. 
samtools sort $file -o ${folder}/$BAMfile'.sorted'.bam

## Remove original file for space optimization.
rm ${folder}/$BAMfile'.bam'
done



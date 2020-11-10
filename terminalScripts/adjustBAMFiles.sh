#!/bin/bash

# Takes path of the folder containing BAM files as argument.

# Initialize a counter variable. If this variable returns 1, we'll make a copy of that file.

countBAM=$(ls $1/*.bam | wc -l) # number of BAM files
BAMfile=$(ls $1/*.bam) # name of BAM file

if [ $countBAM -eq 0 ];
then

echo "No BAM files."
exit 0


elif [ $countBAM -eq 1 ]; 
then

BAMfile=`basename $BAMfile .bam`
cp $1/*.bam $1/$BAMfile'1'.bam
exit 0

fi



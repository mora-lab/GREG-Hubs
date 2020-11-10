#!/bin/bash

## Extracting folder location
folder=$(dirname "${1}")

## Running script
multiBamSummary bins --bamfiles ${1}/*.bam \
--binSize 2000 \
-out ${1}/readCounts.npz \
--outRawCounts ${1}/readCounts.tab
#!/bin/sh  

# donwload the file for analysis
wget https://snap.stanford.edu/data/loc-gowalla_totalCheckins.txt.gz

# unzip the file 
gunzip -k ./loc-gowalla_totalCheckins.txt.gz  

# Evaluate the number of lines 
cat ./loc-gowalla_totalCheckins.txt | wc -l 
# 6442892

# get only the first 10000 rows and the first 4 columns
cut -f 1-4 ./loc-gowalla_totalCheckins.txt | head -n 10000 > gowalla_10klines.txt

# add an indicator variable for above or below 40 latitude using an R script
cat ./gowalla_10klines.txt | ./add_lat_indicator.R > final.txt

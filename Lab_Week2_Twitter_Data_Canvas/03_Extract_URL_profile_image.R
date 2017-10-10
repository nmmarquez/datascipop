### October 2017
### E. Zagheni
### Collect and Analyze Tweets
### Part 3: Extract the URL for the profile picture of Twitter users

#check the directory where you are  
rm(list=ls())
getwd()
## If you are not in the the folder "Collect_Tweets", move to that folder
## using
setwd("~/Documents/datascipop/Lab_Week2_Twitter_Data_Canvas/")

library(rjson)
library(RCurl)

## Read in some Twitter data that you (or someone else) have collected.
## The data would be in json format
## each line is one tweet

data <- readLines("data/tweets_test.json") 


nof_lines <- length(data)
nof_lines

## Apply the function "fromJSON" to each line, one at a time
## to read in the json file and store it as a list
data.list<-lapply(data,function(x) fromJSON(x))

## Alternative approach
data.list2 <- list()
for (i in 1:nof_lines){
data.list2[[i]] <-   fromJSON(data[i])
}

####

data.list[[1]]

data.list[[1]]$user$profile_image_url

data.list[[10]]$user$profile_image_url



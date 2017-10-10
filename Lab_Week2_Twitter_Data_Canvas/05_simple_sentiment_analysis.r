## October 2017
## compute a simple "sentiment" score for
## tweets that mention delta and american airlines 
## Example based on Jeffrey Breen's tutorial:
## https://mran.microsoft.com/posts/twitter.html

rm(list=ls())
setwd("~/Documents/datascipop/Lab_Week2_Twitter_Data_Canvas/")

library(plyr)
library(stringr)

source("sentiment.r")

# import positive and negative words
pos = readLines("opinion_lexicon/positive_words.txt")
neg = readLines("opinion_lexicon/negative_words.txt")

pos
neg

# import tweets for two airlines
data_delta<- read.csv("data/data_delta.csv")
data_AA<- read.csv("data/data_AA.csv")


## Compute a simple sentiment score for each tweet: number of positive words
## minus number of negative words 
scores_delta<- score.sentiment(data_delta$text,pos, neg)$score
scores_AA <- score.sentiment(data_AA$text,pos, neg)$score


mean(scores_delta)
mean(scores_AA)

sd(scores_delta)
sd(scores_AA)


###############
## Extension ##

## Practice collecting Twitter data
## Find two hashtags that are trending at the moment
## Collect tweets that mention those 2 hashtags and evaluate the average and standard deviation for sentiments associated to them, similarly to what we just did
## If you wanted to achieve a confidence interval with a width of 0.01 or less for the mean sentiment for your hashtags, what sample size would you need, respectively?




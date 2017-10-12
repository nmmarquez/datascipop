rm(list=ls())
setwd("~/Documents/datascipop/Lab_Week2_Twitter_Data_Canvas/")

library(ROAuth)
library(twitteR)
library(dplyr)
source('functions_by_pablobarbera.R')
source('./sentiment.r')
## insert here your API key and secret


load(file="./credentials/secretkeys.Rdata")
load("./credentials/twitCred.Rdata")

#setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

sampleStream("data/tweets_hw.json", timeout = 60, 
             oauth = twitCred)

## Read the json file and parse it into a data frame
tweets <- parseTweets("data/tweets_hw.json", verbose = TRUE)
getCommonHashtags(tweets$text, n=10)

tweets_cc <- searchTwitter("@CamilaCabello", n=500)
tweets_L<- searchTwitter("@L", n=500)
#transform your data into a data frame 
data_cc <- twListToDF(tweets_cc)
data_L <- twListToDF(tweets_L)

head(data_cc)


#write the data to disk 
write.csv(data_cc,"data/data_cc.csv")
write.csv(data_L,"data/data_L.csv")


# import positive and negative words
pos <- readLines("opinion_lexicon/positive_words.txt")
neg <- readLines("opinion_lexicon/negative_words.txt")

data_cc <- read.csv("./data/data_cc.csv")
data_L <- read.csv("./data/data_L.csv")

scores_cc <- score.sentiment(data_cc$text,pos, neg)$score
scores_L <- score.sentiment(data_L$text,pos, neg)$score

sent_scores <- data.frame(score=c(scores_cc, scores_L),
                          tag=rep(as.factor(c("CamilaCabello", "L")), each=500))

score_params <- sent_scores %>% dplyr::group_by(tag) %>%
    dplyr::summarize(mean_score=mean(score), sd_score=sd(score))
score_params



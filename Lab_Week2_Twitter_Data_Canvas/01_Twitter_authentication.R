### October 2017
### Emilio Zagheni
### Collect and Analyze Tweets
### Part 1 Twitter Authentication
### For data collection we will use the R package "streamR"
### created by Pablo Barbera


## remove any previously saved objects in your R environment
rm(list=ls())

#check the directory where you are  
getwd()
setwd("~/Documents/datascipop/Lab_Week2_Twitter_Data_Canvas")
## If you are not in the folder "Collect_Tweets", move to that folder
## using the following command
## setwd("YOUR_PATH_TO_THE_FOLDER")
## Alternatively, using the drop-down menu of RStudio
## select Session -> Set Working Directory -> To Source File Location


##Install the packages below and their dependecies
library(devtools)
library(twitteR)
library(streamR)
library(ROAuth)

##parameters and URLs for the Twitter API
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL<- "https://api.twitter.com/oauth/access_token"
authURL<- "https://api.twitter.com/oauth/authorize"

##1. Create a Twitter account: sign up at Twitter.com
##2. If you have a Twitter account, but it is not verified with a phone, 
## you would need to verify the account, by adding a mobile phone number.
## Here is a link to the instructions: https://support.twitter.com/articles/110250 
##3. Create a new app by going to https://apps.twitter.com
##You should leave the "callback website" field empty  
##4.Obtain your consumer key and secret and insert them below 


api_key <- "YOUR CONSUMER KEY GOES HERE"
api_secret<- "YOUR CONSUMER SECRET GOES HERE"
access_token  <- "YOUR ACCESS TOKEN GOES HERE"
access_token_secret <- "YOUR ACCESS SECRET GOES HERE"
#save(api_key, api_secret, access_token, access_token_secret,
#     file="./credentials/secretkeys.Rdata")
load(file="./credentials/secretkeys.Rdata")


# if you need to download the file snag it here
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

twitCred<- OAuthFactory$new(consumerKey=api_key,
                            consumerSecret=api_secret,
                            requestURL=reqURL,
                            accessURL=accessURL,
                            authURL=authURL)

## insert the number in the R console after you run this line
## IMPORTANT: run the following line only, not together with the next line
twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", 
                                        package = "RCurl"))


## Save the authentication object
save(twitCred, file = "credentials/twitCred.Rdata")


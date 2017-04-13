# Install and Activate Packages
library(streamR)
library(RCurl)
library(RJSONIO)
library(stringr)
##############################################################################################################################################
# PART 1: Declare Twitter API Credentials & Create Handshake
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "WBEkkHo5eymAhNIp6S5R2jLf9" # From dev.twitter.com
consumerSecret <- "HuZNvLkMVwSfwWBT5YVrAxvNc6oDjkJxLnOi2zln3i6Quw1Owm" # From dev.twitter.com

tw_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

tw_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
### STOP HERE!!! ###
##############################################################################################################################################
# PART 2: Save the my_oauth data to an .Rdata file
save(tw_oauth, file = "tw_oauth.Rdata")
##############################################################################################################################################
library(streamR)
load("tw_oauth.Rdata")
filterStream(file.name = "Data/tweets.json", # Save tweets in a json file
             track = c("#Stockholm", "#Sweden", "#PrayForStockholm"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
             language = "en",
             timeout = 300, # Keep connection alive for 60 seconds
             oauth = tw_oauth) # Use my_oauth file as the OAuth credentials

tweets.df <- parseTweets("Data/tweets.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
##############################################################################################################################################
write.csv2(tweets.df, file="Data/tweets.csv")


library(twitteR)
library(ROAuth)
api_keys <- read.csv2("api_key.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
setup_twitter_oauth(
  consumer_key = api_keys$consumer_key,
  consumer_secret = api_keys$consumer_secret,
  access_token = api_keys$access_token,
  access_secret =  api_keys$access_token_secret )



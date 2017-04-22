# Load Libraries
library(stringr)
library(igraph)
library(streamR)

#get arguments from cmd line
args <- commandArgs(trailingOnly = TRUE)

#get csv file path 
file <- unlist(args[1])

#read csv file to get tweets into dataframe
tweets.df <- read.csv2(file = file, header = TRUE, stringsAsFactors = FALSE)

# from the data frame, we only need the column text to find out which rows are retweets
tweets_text <- tweets.df[,"text"]

#not necessary
#retweets_texts <- grep("(RT)(?:\\b\\W@\\w+)", tweets_text, ignore.case = TRUE, value = TRUE)

# now we need all retweet ids (those lines in tweets.df which are actual retweets)
retweets_ids  <- grep("(RT)(?:\\b\\W@\\w+)", tweets_text, ignore.case = TRUE )

# create list to store user names of retweets
user_originaltw <- as.list( 1:length(retweets_ids))
user_retweet <- as.list(1:length(retweets_ids))


for (i in 1:length(retweets_ids))
{
  #get tweet text
  tweet_text <- tweets_text[[retweets_ids[i]]]
  #find user who posted original tweet
  tweet_original_poster <- str_extract_all(tweet_text, "(RT)(?:\\b\\W@\\w+)" )
  
  #save user who posted original tweet in vector
  user_originaltw[[i]] = gsub("(RT @)", "", tweet_original_poster, ignore.case = TRUE )
  #save user who retweeted in vector
  user_retweet [[i]] = tweets.df[retweets_ids[i],"screen_name"]
                 
}

user_originaltw <- unlist(user_originaltw)
user_retweet <- unlist(user_retweet)

# generate edgelist
edgelist <- cbind(user_retweet, user_originaltw)

rt_graph = graph.edgelist(edgelist)

save(rt_graph, file = paste(c(file, "_graph.RData")))


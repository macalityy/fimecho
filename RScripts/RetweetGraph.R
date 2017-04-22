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
tweets_text <- sapply(tweets.df, function(x) x$getText())

# now we need to identify retweets
# they can be identified by finding the pattern: "RT<whitespace>@<username>"
rt_id("(RT)(?:\b\W@\w+)",tweets_text, ignore.case = TRUE, value = TRUE)



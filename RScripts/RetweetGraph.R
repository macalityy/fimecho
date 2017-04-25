# Load Libraries
library(stringr)
library(igraph)
library(streamR)
library(plyr)

#get arguments from cmd line
args <- commandArgs(trailingOnly = TRUE)

#get csv file path 
file <- unlist(args[1])
filter.value <- as.integer(unlist(args[2]))

#read csv file to get tweets into dataframe
tweets.df <- read.csv2(file = file, header = TRUE, stringsAsFactors = FALSE)

# from the data frame, we only need the column text to find out which rows are retweets
tweets.text <- tweets.df[,"text"]

# now we need all retweet ids (those lines in tweets.df which are actual retweets)
retweets.ids  <- grep("(RT)(?:\\b\\W@\\w+)", tweets.text, ignore.case = TRUE )

# create list to store user names of retweets
user.originaltw <- as.list( 1:length(retweets.ids))
user.retweet <- as.list(1:length(retweets.ids))


for (i in 1:length(retweets.ids))
{
  #get tweet text
  tweet.text <- tweets.text[[retweets.ids[i]]]
  #find user who posted original tweet
  tweet.original.poster <- str_extract_all(tweet.text, "(RT)(?:\\b\\W@\\w+)" )
  
  #save user who posted original tweet in vector
  user.originaltw[[i]] = gsub("(RT @)", "", tweet.original.poster, ignore.case = TRUE )
  
  #save user who retweeted in vector
  user.retweet [[i]] = tweets.df[retweets.ids[i],"screen_name"]
                 
  ##update data frame
  #tweets.df[retweets.ids[i],"is.rt"] <- TRUE
  #tweets.df[retweets.ids[i],"user.tw"] <- gsub("(RT @)", "", tweet.original.poster, ignore.case = TRUE )
}

user.originaltw <- unlist(user.originaltw)
user.retweet <- unlist(user.retweet)

# generate edgelist
edgelist <- cbind(user.retweet, user.originaltw)

#filter 
if( filter.value > 0)
  {
  edgelist.df <- as.data.frame(edgelist)
  
  #get # of ties for each retweeter and original tweeter
  user.originaltw.freq <- count(user.originaltw)
  user.retweet.freq <- count(user.retweet)
  
  #change colnames of data frame to tiesin and tiesout
  colnames(user.originaltw.freq) <- c("x", "ties.in")
  colnames(user.retweet.freq) <- c("x", "ties.out")
  
  ##build a data frame with # of ties for each user
  user.ties.df <- merge(user.originaltw.freq, user.retweet.freq, by.x = "x", by.y = "x", all = TRUE)
  
  #change all NA values to 0
  user.ties.df[is.na(user.ties.df)] <- 0
  
  #now filter users depending on their amount of ties
  user.ties.filtered.df <- subset(user.ties.df, (user.ties.df$ties.in + user.ties.df$ties.out) > filter.value)
  
  #now filter the edgelist correspondingly
  edgelist.df <- subset(edgelist.df,
                        ( edgelist.df$user.originaltw %in% user.ties.filtered.df$x ) &
                        ( edgelist.df$user.retweet %in% user.ties.filtered.df$x) )

  edgelist <- as.matrix(edgelist.df)
  
  #rm(user.originaltw.freq)
  #rm(user.retweet.freq)
}

#create iGraph Object from edgelist
rt.graph = graph.edgelist(edgelist)
  
  
# set filename
current.time <- format(Sys.time(), "%Y_%m_%d_%H_%M")
graph.name <- paste(c("/home/rstudio/fimecho/Graphs/", current.time, ".RData"), collapse = "")
# save Graph in .RData-File
save( rt.graph, file = graph.name )

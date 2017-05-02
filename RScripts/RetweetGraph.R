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

## User Identification in Retweets ##
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
}

user.originaltw <- unlist(user.originaltw)
user.retweet <- unlist(user.retweet)

# generate edgelist
edgelist <- cbind(user.retweet, user.originaltw)

## Apply Filtering Criteria on Edgelist ##
if( filter.value > 0)
  {
  edgelist.df <- as.data.frame(edgelist)
  
  #get # of ties for each retweeter and original tweeter
  user.originaltw.freq <- count(user.originaltw)
  user.retweet.freq <- count(user.retweet)
  
  #change colnames of data frame to tiesin and tiesout
  colnames(user.originaltw.freq) <- c("x", "ties.in")
  colnames(user.retweet.freq) <- c("x", "ties.out")
  
  #build a data frame with # of ties for each user
  user.ties.df <- merge(user.originaltw.freq,
                        user.retweet.freq,
                        by.x = "x", by.y = "x",
                        all = TRUE)
  #change all NA values to 0
  user.ties.df[is.na(user.ties.df)] <- 0
  
  #now filter users depending on their amount of ties
  user.ties.filtered.df <- subset(user.ties.df,
                                  (user.ties.df$ties.in + user.ties.df$ties.out) > filter.value)
  
  # now get user IDs for all users
  user.ties.filtered.df <- merge(user.ties.filtered.df,
                                 tweets.df[,c("user_id_str","screen_name")],
                                 by.x = "x", by.y = "screen_name",
                                 all.x = TRUE, all.y = FALSE)
  
  # since merge expands the data frame, reduce it back to unique ones
  user.ties.filtered.df <- unique(user.ties.filtered.df)
  
  # and reformat the user id from numeric to character
  user.ties.filtered.df$user_id_str <- as.character(user.ties.filtered.df$user_id_str)
  
  # filter out those users with userid = 'NA'
  user.ties.filtered.df <- subset(user.ties.filtered.df,
                                  is.na(user.ties.filtered.df$user_id_str) == FALSE)
  
  colnames(user.ties.filtered.df) <- c("name", "ties.in", "ties.out", "Id")
  
  # now filter the edgelist correspondingly
  edgelist.df <- subset(edgelist.df,
                        ( edgelist.df$user.originaltw %in% user.ties.filtered.df$name ) &
                        ( edgelist.df$user.retweet %in% user.ties.filtered.df$name) )
  
  
  ### update the ties.in and ties.out
  user.originaltw.freq <- count(edgelist.df$user.originaltw)
  user.retweet.freq <- count(edgelist.df$user.retweet)
  #change colnames of data frame to tiesin and tiesout
  colnames(user.originaltw.freq) <- c("x", "ties.in")
  colnames(user.retweet.freq) <- c("x", "ties.out")
  
  #build a data frame with # of ties for each user
  user.ties.filtered.df <- merge(user.originaltw.freq,
                                 user.retweet.freq,
                                 by.x = "x", by.y = "x",
                                 all = TRUE)
  #change all NA values to 0
  user.ties.filtered.df[is.na(user.ties.filtered.df)] <- 0
  ### / update the ties.in and ties.out
  
  #in edgelist get userIDs for retweeter
  edgelist.id.df <- merge(edgelist.df,
                          user.ties.filtered.df[,c("name", "Id")],
                          by.x = "user.retweet", by.y = "name",
                          all.x = TRUE)
  #in edgelist get userIDs for original poster
  edgelist.id.df <- merge(edgelist.id.df,
                          user.ties.filtered.df[,c("name", "Id")],
                          by.x = "user.originaltw", by.y = "name",
                          all.x = TRUE)
  
  # rename colums
  colnames(edgelist.id.df) <- c("user.to.name", "user.from.name", "source", "target")
  
  #get edgelist with IDs as matrix
  edgelist <- as.matrix( edgelist.id.df[, c("user.to.name", "user.from.name")] )
}


#Create CSV File for Gephi
### STOP
# define a file name!

#file.name <- ""
write.csv2(edgelist.id.df, file = file.name)
#change filename here!
write.csv2(user.ties.filtered.df, file = file.name)
### CONTINUE

#create iGraph Object from edgelist
rt.graph = graph.edgelist(edgelist)
  
  
# set filename
current.time <- format(Sys.time(), "%Y_%m_%d_%H_%M")
graph.name <- paste(c("/home/rstudio/fimecho/Graphs/", current.time, ".RData"), collapse = "")
# save Graph in .RData-File
save( rt.graph, file = graph.name )

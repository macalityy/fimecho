# Load Libraries
library(stringr)
library(igraph)
library(streamR)
library(dplyr)

#get arguments from cmd line
args <- commandArgs(trailingOnly = TRUE)

#get csv file path 
file <- unlist(args[1])

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
                 
}

user.originaltw <- unlist(user.originaltw)
user.retweet <- unlist(user.retweet)

# generate edgelist
edgelist <- cbind(user.retweet, user.originaltw)

#########filte
user.originaltw.grouped <- group_by(edgelist, "user.originaltw")
user.originaltw.freq <- summarise(user.originaltw.grouped, freq = n())

user.originaltw.filtered <- filter(user.originaltw.freq, freq > 10)
user.originaltw.filtered <- user.originaltw.filtered[,1]

edgelist.filtered <- subset(edgelist, user_originaltw %in% user.originaltw.filtered)
########## end of filter


#create iGraph Object from edgelist
rt.graph = graph.edgelist(edgelist)

# set filename
current.time <- format(Sys.time(), "%Y_%m_%d_%H_%M")
graph.name <- paste(c("/home/rstudio/fimecho/Graphs/", current.time, ".RData"), collapse = "")
# save Graph in .RData-File
save( rt.graph, file = graph.name )


grouped.original


